module REPL where

import Expr
import Parsing

data LState = LState { vars :: [(Name, Value)] }

initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val env = (name, val) : filter ((/= name) . fst) env



-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar name = filter ((/= name) . fst)


process :: LState -> Command -> IO ()
process st (Set var Input) = do
    userInput <- getLine  -- Directly read user input without prompting
    let val = StrVal userInput  -- Treat input as a string value
    let st' = LState $ updateVars var val (vars st)
    repl st'
process st (Set var expr) = case eval (vars st) expr of
    Just val -> do
        let st' = LState $ updateVars var val (vars st)
        repl st'
    Nothing -> do
        putStrLn "Error: Evaluation failed."
        repl st
process st (Print expr) = case eval (vars st) expr of
    Just val -> do
        print val
        repl st
    Nothing -> do
        putStrLn "Error: Evaluation failed."
        repl st
process st Quit = putStrLn "Exiting code..."



-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do putStr ("> ")
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error"
                          repl st