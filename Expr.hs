module Expr where

import Parsing

type Name = String

-- At first, 'Expr' contains only addition and values. You will need to
-- add other operations, and variables
data Expr
  = Add Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Subt Expr Expr
  | Val Int
  | Var Name
  deriving (Show)

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command
  = Set Name Expr
  | Eval Expr
  deriving (Show)


-- Helper function to calculate addition, subtraction and multiplication operations
operationCalc :: [(Name, Int)] -> Expr -> Expr -> (Int -> Int -> Int) -> Maybe Int
operationCalc vars x y operator = 
 case (eval vars x, eval vars y) of
        (Just a, Just b) -> Just (operator a b)
        _                -> Nothing

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

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand =
  do
    t <- letter
    char '='
    e <- pExpr
    return (Set [t] e)
    ||| do
      e <- pExpr
      return (Eval e)

pExpr :: Parser Expr
pExpr = do
  t <- pTerm
  do
    char '+'
    e <- pExpr
    return (Add t e)
    ||| do
      char '-'
      e <- pExpr
      return (Subt t e)
    ||| return t

pFactor :: Parser Expr
pFactor =
  do
    d <- digit
    return (Val (digitToInt d))
    ||| do
      v <- letter
      error "Variables not yet implemented"
    ||| do
      char '('
      e <- pExpr
      char ')'
      return e

pTerm :: Parser Expr
pTerm = do
  f <- pFactor
  do
    char '*'
    t <- pTerm
    return (Mult f t)
    ||| do
      char '/'
      t <- pTerm
      return (Div f t)
    ||| return f
