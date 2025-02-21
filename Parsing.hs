{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 |||

stringLit :: Parser String
stringLit = do
  char '"'                          -- Expect opening quote
  content <- many (sat (/= '"'))    -- Parse everything until closing quote
  char '"'                          -- Expect closing quote
  return content
{-
The monad of parsers
--------------------
-}

newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
   fmap f p = do p' <- p
                 return (f p')

instance Applicative Parser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   empty = mzero
   p <|> q = p ||| q

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()
{-
Ignoring spacing
----------------
-}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

intTok :: Parser Int
intTok = token integer

doubleTok :: Parser Double
doubleTok = token double

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

double :: Parser Double
double = do
  -- Parse optional negative sign
  sign <- optional (char '-')

  -- Parse whole number part (at least one digit)
  whole <- many1 digit

  -- Parse optional decimal part
  decimal <- optional $ do
    char '.'
    digits <- many digit  -- Allow empty fractional part (e.g., "3.")
    return digits

  -- Combine parsed components into a number string
  let signStr = case sign of
                  Just '-' -> "-"
                  _        -> ""
      decimalStr = case decimal of
                     Just d -> '.' : d
                     Nothing -> ""
      numStr = signStr ++ whole ++ decimalStr

  -- Convert to Double or fail
  case reads numStr of
    [(x, "")] -> return x
    _         -> failure

{-
New type to handle integers and floats
----------------

data numericalVal
   = Integer Int
   | Double Double
   | String String
   deriving (Show, Eq)

-}