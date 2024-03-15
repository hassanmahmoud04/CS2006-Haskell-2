module REPL where

import Expr
import Parsing
import System.IO (readFile)
import Control.Monad (foldM)

data LState = LState { vars :: VarTree }

initLState :: LState
initLState = LState Empty

-- Helper function to parse commands
parseCommand :: String -> Either String Command
parseCommand line = case parse pCommand line of
    [(cmd, "")] -> Right cmd
    _ -> Left "Parse error."

-- Process commands: Set, Print, and ReadFile
process :: LState -> Command -> IO LState
process st (Set var expr) = do
    let valResult = eval (vars st) expr
    case valResult of
        Right val -> do
            let updatedVars = insert var val (vars st)
            return st { vars = updatedVars }
        Left errMsg -> do
            putStrLn errMsg
            return st

process st (Print expr) = do
    let valResult = eval (vars st) expr
    case valResult of
        Right val -> do
            print val
            return st
        Left errMsg -> do
            putStrLn errMsg
            return st

process st (ReadFile filePath) = do
    contents <- readFile filePath
    let linesOfCommands = lines contents
    foldM processCommand st linesOfCommands
  where
    processCommand accSt line = case parseCommand line of
        Right cmd -> process accSt cmd
        Left errorMsg -> putStrLn ("Error processing command from file: " ++ errorMsg) >> return accSt

-- REPL loop
repl :: LState -> IO ()
repl st = do
    putStr "> "
    inp <- getLine
    case parseCommand inp of
        Right cmd -> do
            newState <- process st cmd
            repl newState
        Left errorMsg -> do
            putStrLn ("Error: " ++ errorMsg)
            repl st
