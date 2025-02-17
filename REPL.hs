module REPL where

import Expr
import Parsing

data REPLState = REPLState { vars :: [(Name, Int)],
                             history :: [Command],
                             commandNo :: Int }

initREPLState :: REPLState
-- Ensures that the command count begins from 0
initREPLState = REPLState [] [] 0


-- Function which given a variable name and a value, returns a new set of variables with
-- that name and value added.
-- If it already exists, removes the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars name value vars = (name, value) : dropVar name vars


-- Function which returns a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar name vars = [ (n, v) | (n, v) <- vars, n /= name ]


-- Function which adds a command to the command history in the state
addHistory :: REPLState -> Command -> REPLState
addHistory st command = st { history = history st ++ [command], commandNo = commandNo st + 1 }


-- Helper function which attempts to evaluate the expression and produce a result
handleEvalResult :: Maybe Int -> REPLState -> IO REPLState
handleEvalResult (Just value) st = do
  -- Prints the result if the evaluation is successful
  putStrLn (show value)
  return st

handleEvalResult Nothing st = do
  -- Prints an error message if the evaluation is not successful
  putStrLn "There has been an error in this evaluation due to an invalid expression or missing variable."
  return st

-- Function which processes a set command by setting the variable and updating the state
process :: REPLState -> Command -> IO ()
process st (Set var e)
-- Ensures that 'it' cannot be overwritten and will only store the result of the most recent calculation
  | var == "it" = do
      putStrLn "You cannot assign a value to the implicit variable 'it'."
      repl st
  | otherwise = do
      let eResult = eval (vars st) e
      st' <- case eResult of
               Nothing -> do
                 putStrLn "The variable was not assigned successfully, try again."
                 return st
               Just value -> do
                 putStrLn "OK"
                 return st { vars = updateVars var value (vars st) }
      let newState = addHistory st' (Set var e)
      repl newState

-- Function to evaluate an expression and print the result or an error message
process st (Eval e) = do
  let eResult = eval (vars st) e
  st' <- handleEvalResult eResult st
  let st'' = case eResult of
              -- Updates the value of 'it' to store the result of the most recent calculation
               Just value -> st' { vars = updateVars "it" value (vars st') }
               Nothing    -> st'
  let newState = addHistory st'' (Eval e)
  repl newState

  -- Function to return the result associated with a specified command number
process st (Recall n) = do
  let historyLength = length (history st)
  if n >= 0 && n < historyLength
    then do
      let cmd = history st !! n
      process st cmd
    else do
      putStrLn "The command number you have entered is invalid."
      repl st

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: REPLState -> IO ()
repl st = do putStr (show (length (history st)) ++ " > ")
             inp <- getLine
            -- Allows for the user to quit the calculator gracefully
             if inp == ":q"
              then putStrLn "Bye"
             else case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "There has been a parse error."
                          repl st
