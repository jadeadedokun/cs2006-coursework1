module REPL where

import Expr
import Parsing
import System.IO (hFlush, stdout)
import Control.Monad (foldM)
import System.IO.Error (catchIOError, ioeGetErrorString)


data REPLState = REPLState { vars :: BST Name Value  
                           , history :: [Command]
                           , commandNo :: Int }

initREPLState :: REPLState
initREPLState = REPLState Empty [] 0 

-- Function which given a variable name and a value, returns a new set of variables with
-- that name and value added.
-- If it already exists, removes the old value
updateVars :: Name -> Value -> BST Name Value -> BST Name Value
updateVars = bstInsert 

dropVar :: Name -> BST Name Value -> BST Name Value
dropVar = bstDelete

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
process :: REPLState -> Command -> IO REPLState
-- Ensures that 'it' cannot be overwritten and will only store the result of the most recent calculation 
process st (Set "it" _) = do
  putStrLn "You cannot assign a value to the implicit variable 'it'."
  return st

process st (Set var e) = do
  case eval (vars st) e of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return st  -- Return current state without updating
    Right value -> do
      putStrLn "OK"
      let newVars = bstInsert var value (vars st)  -- Use BST
      let newState = st { vars = newVars }
      -- Add the Set command to the history
      let updatedState = addHistory newState (Set var e)
      return updatedState

process st (ReadFile path) = do
  -- Read file contents and handle IO errors
  contents <- readFile path `catchIOError` (\e -> do
    putStrLn $ "File error: " ++ ioeGetErrorString e
    return "")
  let lines' = lines contents
  -- Parse and execute each line
  foldM processLine st lines'
  where
    processLine st' line = case parse pCommand line of
      [(cmd, "")] -> process st' cmd  -- Valid command
      _ -> do
        putStrLn $ "Parse error in line: " ++ line
        return st'  -- Continue processing subsequent commands

-- Function to return the result associated with a specified command number if it exists 
process st (Recall n) = do
  let historyLength = length (history st)
  if n >= 0 && n < historyLength
    then process st (history st !! n)
    else do
      putStrLn "The command number you have entered is invalid."
      return st

-- Function to evaluate an expression and print the result or an error message 
process st (Eval e) = do
  case eval (vars st) e of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return st
    Right value -> do
      putStrLn $ show value
    -- Updates the value of 'it' to store the result of the most recent numerical calculation 
      let newVars = updateVars "it" value (vars st)
      let newState = addHistory st { vars = newVars } (Eval e)
      return newState

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.
repl :: REPLState -> IO ()
repl st = do putStr (show (length (history st)) ++ " > ")
            -- Prevents the user input from appearing twice
             hFlush stdout
             inp <- getLine 
            -- Allows for the user to quit the calculator gracefully 
             case inp of  
              ":q" -> putStrLn "The calculator has ended." 
              _ -> case parse pCommand inp of 
                  [(cmd, "")] -> do -- Must parse entire input 
                          newState <- process st cmd
                          repl newState
                  _ -> do putStrLn "There has been a parse error." 
                          repl st
