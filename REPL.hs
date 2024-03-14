module REPL where

import Expr
import Parsing

-- Adjusting LState to handle a more flexible value type, accommodating Floats.
data Value = IntVal Int | FloatVal Float deriving Show
data LState = LState { vars :: [(Name, Value)] }

initLState :: LState
initLState = LState []

-- Implement updateVars to properly handle adding or updating variables
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val [] = [(name, val)]
updateVars name val (x:xs)
    | fst x == name = (name, val) : xs
    | otherwise = x : updateVars name val xs

-- Implement dropVar to remove a variable from the state
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name = filter (\(n,_) -> n /= name)

-- Processing commands, adjusted for new operations and types
process :: LState -> Command -> IO ()
process st (Set var e) 
     = case eval (vars st) e of
          Just val -> let vars' = updateVars var (FloatVal val) (vars st) -- Assuming all eval results are Float for simplicity
                      in repl (LState vars')
          Nothing -> putStrLn "Error: Evaluation failed." >> repl st
process st (Print e) 
     = case eval (vars st) e of
          Just val -> putStrLn ("Result: " ++ show val) >> repl st
          Nothing -> putStrLn "Error: Evaluation failed." >> repl st

-- The main REPL loop, parsing commands and handling them
repl :: LState -> IO ()
repl st = do putStr "> "
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> process st cmd
                  _ -> putStrLn "Error: Parse error." >> repl st
