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
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name = filter ((/= name) . fst)

process :: LState -> Command -> IO ()
process st (Set var e) = case eval (vars st) e of
     Just val -> do
          let st' = LState $ updateVars var val (vars st)
          repl st'
     Nothing -> do
          putStrLn "Error: Command evaluation failed."
          repl st
process st (Print e) = case eval (vars st) e of
     Just val -> do
          putStrLn (show val)
          repl st
     Nothing -> do
          putStrLn "Error: Command evaluation failed."
          repl st
process st Quit = do return ()

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
