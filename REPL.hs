module REPL where

import Expr
import Parsing
import Data.HashMap
import Data.Tuple
import System.IO

data LState = LState { vars :: Map Name Value }

initLState :: LState
initLState = LState empty

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> Map Name Value -> Map Name Value
updateVars name val env = insert name val (dropVar name env)



-- Return a new set of variables with the given name removed
dropVar :: Name -> Map Name Value -> Map Name Value
dropVar name env = delete name env


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
        putStrLn "Error: Evaluation failed. Not a valid type for a variable."
        repl st
process st (Print expr) = case eval (vars st) expr of
    Just val -> do
        print val
        repl st
    Nothing -> do
        putStrLn "Error: Evaluation failed. Invalid expression to be printed, please check that operations are only performed on homogenous types."
        repl st
process st Quit = do 
            putStrLn "Exiting code..."
            return ()
process st (Read path) = do
    let concatPath = Prelude.filter (/='"') ("./" ++ show path ++ ".txt")
    putStrLn ("Reading from file:" ++ (show concatPath))
    file <- readFile (concatPath)
    let allLines = lines file
    let parsedLines = Prelude.map (parse pCommand) (allLines)
    let cmds = Prelude.map (Data.Tuple.fst) (Prelude.map head parsedLines)
    let sts = (Prelude.map (readRepl st) (cmds))
    walkthrough sts cmds
    repl (last sts)
process st (If c t e) = case eval (vars st) c of
    Just (IntVal 1) -> do
        process st t
    Just (IntVal 0) -> do 
        process st e
    Nothing -> do
        putStrLn "Error: Conditional statement failed. Usage: If <condition> then <command> else <command>."
        repl st


readRepl :: LState -> Command -> LState
readRepl st (Set var expr) = case eval (vars st) expr of
    Just val -> LState $ updateVars var val (vars st)
    Nothing -> st
readRepl st (Print expr) = case eval (vars st) expr of
    Just val -> st
    Nothing -> st

walkthrough :: [LState] -> [Command] -> IO ()
walkthrough [] []    = pure ()
walkthrough (x:xs) (y:ys) = do process x y

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do putStr "> "
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error"
                          repl st