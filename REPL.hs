{-# LANGUAGE BlockArguments #-}

module REPL where

import Expr
import Parsing(parse)
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


process :: REPLState -> Command -> IO REPLState
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
      return $ st { vars = newVars }

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

process st (Recall n) = do
  let historyLength = length (history st)
  if n >= 0 && n < historyLength
    then process st (history st !! n)
    else do
      putStrLn "Invalid command number."
      return st

process st (Eval e) = do
  case eval (vars st) e of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return st
    Right value -> do
      putStrLn $ show value
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
            -- The following line was inspired by: https://mail.haskell.org/pipermail/beginners/2010-March/003692.htmlma
             hFlush stdout
             inp <- getLine
            -- Allows for the user to quit the calculator gracefully
             case inp of 
              ":q" -> putStrLn "Bye"
              _ -> case parse pCommand inp of
                  [(cmd, "")] -> do -- Must parse entire input
                     newState <- process st cmd
                     repl newState 
                  _ -> do putStrLn "There has been a parse error."
                          repl st
