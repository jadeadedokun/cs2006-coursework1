module REPL where

import Expr
import Parsing

data REPLState = REPLState { vars :: [(Name, Int)],
                             history :: [Command] }

initREPLState :: REPLState
initREPLState = REPLState [] []


-- Function which given a variable name and a value, returns a new set of variables with
-- that name and value added.
-- If it already exists, removes the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]

-- Function which checks if the set of variables contains the specified name and deletes it if it does, otherwise adds in the name and value
updateVars name value vars = (name, value) : dropVar name vars


-- Function which returns a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar name vars = [ (n, v) | (n, v) <- vars, n /= name ]


-- Function which adds a command to the command history in the state
addHistory :: REPLState -> Command -> REPLState
addHistory st command = st { history = command : history st }


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

-- Process 'Set' command: Set the variable, update the state, but no printing of result
process :: REPLState -> Command -> IO ()
process st (Set var e) = do
  let eResult = eval (vars st) e
  let st' = case eResult of
        Just value -> st { vars = updateVars var value (vars st) }
        Nothing    -> st
-- Calls the addHistoryFunction to add the expression to history
  let newState = addHistory st' (Set var e)
  repl newState

-- Process 'Eval' command: Evaluate the expression and print the result or error message
process st (Eval e) = do
  let eResult = eval (vars st) e
  st' <- handleEvalResult eResult st
  let newState = addHistory st' (Eval e)
  repl newState

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
                  _ -> do putStrLn "Parse error"
                          repl st
