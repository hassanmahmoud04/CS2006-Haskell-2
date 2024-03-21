module REPL where

import Expr
import Parsing

data LState = LState { vars :: [(Name, Int)] }

initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars name val [] = [(name, val)]
updateVars name val ((k,v):xs) = if name == k
                                    then ((k,val):xs)
                                    else updateVars name val xs

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar = undefined

process :: LState -> Command -> IO ()
<<<<<<< Updated upstream
process st (Set var e) 
     = do let st' = undefined
          -- st' should include the variable set to the result of evaluating e
          repl st'
process st (Print e) 
     = do let st' = undefined
          -- Print the result of evaluation
          repl st'
process st Quit = do return ()
=======
process st (Set var e) = do 
     let val = eval (vars st) e
     case val of
          Nothing -> do
               putStrLn ("Error: ")
               repl st
          Just val -> do
               let st' = LState ( updateVars var val (vars st))
               repl st'
     -- st' should include the variable set to the result of evaluating e
          
process st (Print e) = do
    let evalResult = eval (vars st) e
    case evalResult of
        Just val -> do
               putStrLn (show val)
               repl st
        Nothing -> do
               putStrLn ("Error: ")
               repl st
          
>>>>>>> Stashed changes

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
