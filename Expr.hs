module Expr where

import Parsing

type Name = String

-- Expr data type which defines the variables present in different expression calculations
data Expr
  = Add Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Subt Expr Expr
  | Abs Expr
  | Mod Expr Expr
  | Pow Expr Expr
  | Val Int
  | Var Name
  deriving (Show)

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command
  = Set Name Expr
  | Eval Expr
  | Recall Int
  deriving (Show)


-- Helper function to calculate addition, subtraction and multiplication operations
operationCalc :: [(Name, Int)] -> Expr -> Expr -> (Int -> Int -> Int) -> Maybe Int
operationCalc vars x y operator = 
 case (eval vars x, eval vars y) of
        (Just a, Just b) -> Just (operator a b)
        _                -> Nothing

-- Function to calcuate the absolute value of a number (if negative, otherwise returns the same number)
absCalc :: [(Name, Int)] -> Expr -> Maybe Int
absCalc vars x = 
  case eval vars x of
    Just value -> Just (abs value)
    _    -> Nothing

eval ::
  [(Name, Int)] -> -- Variable name to value mapping
  Expr -> -- Expression to evaluate
  Maybe Int -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
-- If the searched for name is found, a value is returned or nothing (so that an appropriate error message can be given)
eval vars (Var name) = lookup name vars
-- If both expressions are valid, return the calculation otherwise return nothing (so that an appropriate error message can be given)
eval vars (Add x y) = operationCalc vars x y (+)
eval vars (Subt x y) = operationCalc vars x y (-)
eval vars (Mult x y) = operationCalc vars x y (*)
-- Handles the special case of division by 0 as well as all other division operations
eval vars (Div x y) = 
  case (eval vars x, eval vars y) of
    (Just a, Just 0) -> Just 0
    (Just a, Just b) -> Just (a `div` b)
    _ -> Nothing
eval vars (Mod x y) = operationCalc vars x y mod
eval vars (Pow x y) = operationCalc vars x y (^)
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
    return (Set t e)
  ||| do
    skipExtraChars
    e <- pExpr
    skipExtraChars
    return (Eval e)

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
    -- Checks if there is a minus sign before a number in order to identify it as negative
    char '-'
    skipExtraChars
    n <- many1 digit
    return (Val (-(read n)))
    ||| do    
    skipExtraChars
    n <- many1 digit
    return (Val (read n))
    ||| do
      skipExtraChars
      -- Allows for calculations to be performed with variables of names longer than one letter
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
    string "mod"
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
