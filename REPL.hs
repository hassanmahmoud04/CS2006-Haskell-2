module REPL where

import Expr -- This imports the Value type from Expr.hs
import Parsing

-- No longer need to define Value here since it's imported from Expr
data LState = LState { vars :: [(Name, Value)] }

initLState :: LState
initLState = LState []

updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val [] = [(name, val)]
updateVars name val (x:xs)
    | fst x == name = (name, val) : xs
    | otherwise = x : updateVars name val xs

dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name = filter (\(n, _) -> n /= name)

process :: LState -> Command -> IO ()
process st (Set var expr) = do
    let evalResult = eval (vars st) expr
    case evalResult of
        Right val -> do
            let vars' = updateVars var val (vars st)
            repl (LState vars')
        Left errorMsg -> do
            putStrLn ("Error: " ++ errorMsg)
            repl st
process st (Print expr) = do
    let evalResult = eval (vars st) expr
    case evalResult of
        Right val -> do
            putStrLn ("Result: " ++ show val)
            repl st
        Left errorMsg -> do
            putStrLn ("Error: " ++ errorMsg)
            repl st

repl :: LState -> IO ()
repl st = do
    putStr "> "
    inp <- getLine
    case parse pCommand inp of
        [(cmd, "")] -> process st cmd
        _ -> do
            putStrLn "Error: Parse error."
            repl st
