module REPL where

import Expr
import Parsing (parse)

data REPLState = REPLState { vars :: [(Name, Value)],  -- Stores Int, Double, or String
                             history :: [Command],
                             commandNo :: Int }

initREPLState :: REPLState
-- Ensures that the command count begins from 0
initREPLState = REPLState [] [] 0

-- Function which given a variable name and a value, returns a new set of variables with
-- that name and value added.
-- If it already exists, removes the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name value vars = (name, value) : dropVar name vars

dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name vars = [ (n, v) | (n, v) <- vars, n /= name ]

-- Function which adds a command to the command history in the state
addHistory :: REPLState -> Command -> REPLState
addHistory st command = st { history = history st ++ [command], commandNo = commandNo st + 1 }

-- Helper function which attempts to evaluate the expression and produce a result
handleEvalResult :: Either String Value -> REPLState -> IO REPLState
handleEvalResult (Right value) st = do
  -- Prints the result if the evaluation is successful
  putStrLn $ "Result: " ++ show value
  return st
handleEvalResult (Left err) st = do
  -- Prints an error message if the evaluation is not successful
  putStrLn $ "Error: " ++ err
  return st

-- Function which processes a set command by setting the variable and updating the state
process :: REPLState -> Command -> IO ()
process st (Set var e)
-- Ensures that 'it' cannot be overwritten and will only store the result of the most recent calculation
  | var == "it" = do
      putStrLn "You cannot assign a value to the implicit variable 'it'."
      repl st
  | otherwise = do
      case eval (vars st) e of
        Left err -> do
          putStrLn $ "Assignment error: " ++ err
          repl st
        Right value -> do
          putStrLn "OK"
          let newVars = updateVars var value (vars st)
          let newState = addHistory st { vars = newVars } (Set var e)
          repl newState

-- Function to evaluate an expression and print the result or an error message
process st (Recall n) = do
  let historyLength = length (history st)
  if n >= 0 && n < historyLength
    then do
      let cmd = history st !! n
      process st cmd
    else do
      putStrLn "Invalid command number."
      repl st

-- Function to return the result associated with a specified command number
process st (Eval e) = do
  case eval (vars st) e of
    Left err -> do
      putStrLn $ "Error: " ++ err
      repl st
    Right value -> do
      putStrLn $  show value
      let newVars = updateVars "it" value (vars st)
      let newState = addHistory st { vars = newVars } (Eval e)
      repl newState

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.
repl :: REPLState -> IO ()
repl st = do putStr (show (length (history st)) ++ " > ")
             inp <- getLine
            -- Allows for the user to quit the calculator gracefully
             case inp of 
              ":q" -> putStrLn "Bye"
              _ -> case parse pCommand inp of
                  [(cmd, "")] -> do -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error."
                          repl st