module Expr where

import Parsing

type Name = String

-- show instance for value type to be convert values to their string representation
instance Show Value where
  show (IntVal x) = show x
  show (DoubleVal x) = show x
  show (StringVal s) = s

-- Expr data type which defines the variables present in different expression calculations
data Expr
  = Add Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Subt Expr Expr
  | Abs Expr
  | Mod Expr Expr
  | Pow Expr Expr
  | Val Value  -- supports Int, Double, and String
  | Var Name
  deriving (Show)

data Value = IntVal Int | DoubleVal Double | StringVal String deriving (Eq)

data BST k v = Empty | Node (BST k v) k v (BST k v) deriving (Show)

-- These are the REPL commands - set a variable name to a value, evaluate an expression, repeat the result of an earlier expressionj
data Command
  = Set Name Expr
  | Eval Expr
  | Recall Int
  | ReadFile FilePath
  deriving (Show)

  -- Insert a key-value pair into the BST
bstInsert :: Ord k => k -> v -> BST k v -> BST k v
bstInsert k v Empty = Node Empty k v Empty
bstInsert k v (Node left k' v' right)
  | k < k'    = Node (bstInsert k v left) k' v' right
  | k > k'    = Node left k' v' (bstInsert k v right)
  | otherwise = Node left k v right  -- Replace existing value
-- Lookup a key in the BST


-- lookup key in the BST
bstLookup :: Ord k => k -> BST k v -> Maybe v
bstLookup _ Empty = Nothing
bstLookup k (Node left k' v' right)
  | k < k'    = bstLookup k left
  | k > k'    = bstLookup k right
  | otherwise = Just v' 

--Deletes keys from the BST, remains the same if key is not found 
bstDelete :: Ord k => k -> BST k v -> BST k v
bstDelete _ Empty = Empty
bstDelete k (Node left k' v' right)
  | k < k'    = Node (bstDelete k left) k' v' right
  | k > k'    = Node left k' v' (bstDelete k right)
  | otherwise = case (left, right) of
      (Empty, Empty) -> Empty
      (Empty, _)     -> right
      (_, Empty)     -> left
      _              -> let (minK, minV) = bstMin right
                        in Node left minK minV (bstDelete minK right)
  where
    bstMin (Node Empty k v _) = (k, v)
    bstMin (Node left _ _ _)  = bstMin left

-- Helper to extract numerical value (Int or Double)
getNum :: Value -> Either String Double
getNum (IntVal x) = Right (fromIntegral x)
getNum (DoubleVal x) = Right x
getNum _ = Left "Please input a number."

-- Helper to check if two values are compatible for arithmetic
bothNums :: Value -> Value -> Either String (Double, Double)
bothNums a b = do
  x <- getNum a
  y <- getNum b
  return (x, y)

-- Helper function to calculate addition, subtraction and multiplication operations
operationCalc :: BST Name Value -> Expr -> Expr -> (Double -> Double -> Double) -> Either String Value
operationCalc vars x y operator = do
  a <- eval vars x
  b <- eval vars y
  (a', b') <- bothNums a b
  return $ DoubleVal (operator a' b')

-- Function to calcuate the absolute value of a number (if negative, otherwise returns the same number) 
absCalc :: BST Name Value -> Expr -> Either String Value
absCalc vars x = do
  a <- eval vars x
  case a of
    IntVal n    -> Right $ IntVal (abs n)
    DoubleVal d -> Right $ DoubleVal (abs d)
    _           -> Left "Cannot take absolute value of a string."

-- Returns a value directly if it is given
eval :: BST Name Value -> Expr -> Either String Value
eval _ (Val x) = Right x

-- Searches for a name and returns it if found or a suitable error message
eval vars (Var name) = case bstLookup name vars of
  Just v  -> Right v
  Nothing -> Left $ "Variable '" ++ name ++ "' not found."

-- Handles addition and outputs a correct value or error message if necessary
eval vars (Add x y) = do
  a <- eval vars x
  b <- eval vars y
  case (a, b) of
    (StringVal s1, StringVal s2) -> Right $ StringVal (s1 ++ s2)
    (StringVal _, _) -> Left "Cannot add string to number."
    (_, StringVal _) -> Left "Cannot add number to string."
    _ -> do
      (a', b') <- bothNums a b
      if fromIntegral (round a') == a' && fromIntegral (round b') == b'
        then Right $ IntVal (round a' + round b')
        else Right $ DoubleVal (a' + b')

-- Handles subtraction and outputs a correct value or error message if necessary
eval vars (Subt x y) = operationCalc vars x y (-)

-- Handles multiplication (with strings and ) and outputs a correct value or error message if necessary
eval vars (Mult x y) = do
  a <- eval vars x
  b <- eval vars y
  case (a, b) of
    (StringVal _, _) -> Left "Cannot multiply strings."
    (_, StringVal _) -> Left "Cannot multiply by strings."
    _ -> operationCalc vars x y (*)

-- Handles the special case of division by 0 as well as all other division operations 
eval vars (Div x y) = do
  aVal <- eval vars x 
  bVal <- eval vars y  
  (a, b) <- bothNums aVal bVal  
  if b == 0 
    then Left "Division by zero." 
    else Right $ DoubleVal (a / b)

eval vars (Mod x y) = do
  a <- eval vars x
  b <- eval vars y
  case (a, b) of
    (IntVal a', IntVal b') -> 
      if b' == 0 
        then Left "Modulus by zero." 
        else Right $ IntVal (a' `mod` b')
    _ -> Left "Modulus requires integer operands."

eval vars (Pow x y) = operationCalc vars x y (**)

eval vars (Abs x) = absCalc vars x

-- Function to skip comments that are added on the command line by consuming them until a new line is reached
skipComments :: Parser ()
skipComments = do
   (char '#' >> many (sat (/= '\n')) >> return ()) ||| return ()
   (string "--" >> many (sat (/= '\n')) >> return ()) ||| return ()

-- Function that allows for extra characters (comments and spaces) to be skipped
skipExtraChars :: Parser ()
skipExtraChars = do
  space
  skipComments

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

-- Parser for commands (supports absolute value, recall, set, and eval)
pCommand :: Parser Command
pCommand =
    -- Allows for parsing the absolute value of the result of an expression 
  (do skipExtraChars
      string "Read"
      skipExtraChars
      path <- many1 (sat (/= ' '))
      return (ReadFile path))
  ||| (do skipExtraChars
          char '|'
          skipExtraChars
          e <- pExpr
          skipExtraChars
          char '|'
          skipExtraChars
          return (Eval (Abs e)))
    -- Allows for numbers greater than one digit to be entered into the calculator for use 
  ||| do
    skipExtraChars
    char '!'
    -- Allows for numbers greater than one digit to be entered into the calculator for use
    n <- integer
    return (Recall (n))
  ||| do
    skipExtraChars
    t <- ident -- Allow digits in variable names (e.g., x0, x1)
    skipExtraChars
    char '='
    skipExtraChars
    e <- pExpr
    skipExtraChars
    return (Set t e)  -- Set variable t to the result of expression e
  ||| do
    skipExtraChars
    e <- pExpr
    skipExtraChars
    return (Eval e)

-- Parser for expressions (terms and factors)
pExpr :: Parser Expr
pExpr = do
  t <- pTerm
  do
    skipExtraChars
    char '+'
    e <- pExpr
    skipExtraChars
    return (Add t e)
    ||| do
      skipExtraChars
      char '-'
      e <- pExpr
      skipExtraChars
      return (Subt t e)
    ||| return t

-- Parser for factors (values, variables, parentheses, etc.)
pFactor :: Parser Expr
pFactor =
  do
    skipExtraChars
    char '|'
    skipExtraChars
    e <- pExpr
    skipExtraChars
    char '|'
    skipExtraChars
    return (Abs e)
  ||| do
    skipExtraChars
    s <- stringLit  -- Parse strings
    skipExtraChars
    return (Val (StringVal s))
  ||| do
    skipExtraChars
    d <- doubleTok  -- Parse floating-point numbers first
    skipExtraChars
    return (Val (DoubleVal d))
    ||| do    
    skipExtraChars 
    n <- intTok
    return (Val (IntVal(n)))
    ||| do 
      skipExtraChars 
      -- Allows for calculations to be performed with variables of names containing any alphanumeric characters 
      v <- ident 
      skipExtraChars 
      return (Var v) 
    ||| do 
      skipExtraChars 
      char '(' 
      e <- pExpr 
      char ')' 
      skipExtraChars 
      return e 

-- Parser for terms (multiplication, division, etc.)
pTerm :: Parser Expr 
pTerm = do 
  skipExtraChars 
  f <- pFactor 
  do 
    skipExtraChars 
    char '^' 
    t <- pTerm 
    skipExtraChars 
    return (Pow f t) 
      -- Allows for the modulus calculation to be performed 
    ||| do 
    skipExtraChars 
    symbol "mod" 
    t <- pTerm 
    skipExtraChars 
    return (Mod f t) 
      -- Establishes an alternative symbol which is frequently used to signify modulus 
    ||| do 
    skipExtraChars 
    char '%' 
    t <- pTerm 
    skipExtraChars 
    return (Mod f t)     
    ||| do 
    skipExtraChars 
    char '*' 
    t <- pTerm 
    skipExtraChars 
    return (Mult f t) 
      -- Establishes an alternative symbol which is frequently used to signify multiplication 
    ||| do 
    skipExtraChars 
    char 'x' 
    t <- pTerm 
    skipExtraChars 
    return (Mult f t)     
    ||| do 
    skipExtraChars 
    char '/' 
    t <- pTerm 
    skipExtraChars 
    return (Div f t) 
    ||| return f 