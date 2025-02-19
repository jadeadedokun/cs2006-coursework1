module Expr where


import Parsing (Parser, char, digit, letter, many, many1, sat, string, intTok, doubleTok, stringLit, (|||))
import Data.Char (isSpace, isDigit)

type Name = String

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
  | Val Value  -- Changed to support Int, Double, and String
  | Var Name
  deriving (Show)

data Value = IntVal Int | DoubleVal Double | StringVal String deriving (Eq)



-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command
  = Set Name Expr
  | Eval Expr
  | Recall Int
  deriving (Show)

-- Helper to extract numerical value (Int or Double)
getNum :: Value -> Either String Double
getNum (IntVal x) = Right (fromIntegral x)
getNum (DoubleVal x) = Right x
getNum _ = Left "Expected a number."

-- Helper to check if two values are compatible for arithmetic
bothNums :: Value -> Value -> Either String (Double, Double)
bothNums a b = do
  x <- getNum a
  y <- getNum b
  return (x, y)

-- Helper function to calculate addition, subtraction and multiplication operations
operationCalc :: [(Name, Value)] -> Expr -> Expr -> (Double -> Double -> Double) -> Either String Value
operationCalc vars x y operator = do
  a <- eval vars x
  b <- eval vars y
  (a', b') <- bothNums a b
  return $ DoubleVal (operator a' b')

-- Function to calculate the absolute value of a number
absCalc :: [(Name, Value)] -> Expr -> Either String Value
absCalc vars x = do
  a <- eval vars x
  case a of
    IntVal n    -> Right $ IntVal (abs n)
    DoubleVal d -> Right $ DoubleVal (abs d)
    _           -> Left "Cannot take absolute value of a string."

eval :: [(Name, Value)] -> Expr -> Either String Value
eval _ (Val x) = Right x
eval vars (Var name) = case lookup name vars of
  Just v  -> Right v
  Nothing -> Left $ "Variable '" ++ name ++ "' not found."
eval vars (Add x y) = do
  a <- eval vars x
  b <- eval vars y
  case (a, b) of
    (StringVal s1, StringVal s2) -> Right $ StringVal (s1 ++ s2)  
    _ -> do
      (a', b') <- bothNums a b
      if fromIntegral (round a') == a' && fromIntegral (round b') == b'
        then Right $ IntVal (round a' + round b')
        else Right $ DoubleVal (a' + b')
eval vars (Subt x y) = operationCalc vars x y (-)
eval vars (Mult x y) = operationCalc vars x y (*)
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

skipExtraChars :: Parser ()
skipExtraChars = do
  many (sat isSpace)
  return ()

-- Parser for commands (supports absolute value, recall, set, and eval)
pCommand :: Parser Command
pCommand =
  do
    skipExtraChars
    -- Allows for parsing the absolute value of the result of an expression
    char '|'
    skipExtraChars
    e <- pExpr
    skipExtraChars
    char '|'
    skipExtraChars
    return (Eval (Abs e))
  ||| do
    skipExtraChars
    char '!'
    -- Allows for numbers greater than one digit to be entered into the calculator for use
    n <- many1 digit
    return (Recall (read n))
  ||| do
    skipExtraChars
    t <- many1 letter
    skipExtraChars
    char '='
    skipExtraChars
    e <- pExpr
    skipExtraChars
    return (Set t e)  -- Removed extra ')'
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
    n <- intTok  -- Parse integers after floats
    skipExtraChars
    return (Val (IntVal n))
  ||| do
    skipExtraChars
    v <- many1 letter
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
    ||| do
    skipExtraChars
    string "mod"
    t <- pTerm
    skipExtraChars
    return (Mod f t)
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