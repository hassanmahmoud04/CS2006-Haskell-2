module REPL where

import Expr
import Parsing
import Data.HashMap
import Data.Tuple
import System.IO

import System.Console.Haskeline
import Control.Monad
import Control.Monad (foldM)
import Control.Monad(replicateM_)
import Control.Monad.IO.Class (liftIO) -- For liftIO
import Data.List (isPrefixOf)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class

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


process :: Command -> InputT (StateT LState IO) ()
process (Repeat n cmds) = replicateM_ n (mapM_ process cmds)
process (Set var Input) = do
    st <- lift get
    minput <- getInputLine ""  -- Directly read user input without prompting
    case minput of
        Nothing -> return ()
        Just userInput -> do
            let val = StrVal(userInput)
            let updatedVars = updateVars var val (vars st)
            lift $ put st{ vars = updatedVars }
process (Set var expr) = do
    st <- lift get -- Lift the get operation from StateT into InputT (StateT IO LState)
    case eval (vars st) expr of
        Just val -> do
            let updatedVars = updateVars var val (vars st)
            lift $ put st{ vars = updatedVars }  -- Lift the put operation to update state
        Nothing -> outputStrLn "Error: Evaluation failed."
process (Print expr) = do
    st <- lift get  -- Access the current state
    case eval (vars st) expr of
        Just val -> outputStrLn $ show val
        Nothing -> outputStrLn "Error: Evaluation failed."
process (Read path) = do
    st <- lift get
    let concatPath = Prelude.filter (/='"') ("./" ++ show path ++ ".txt")
    outputStrLn ("Reading from file:" ++ (show concatPath))
    file <- lift ( lift(readFile (concatPath)))
    let allLines = lines file
    let parsedLines = Prelude.map (parse pCommand) (allLines)
    let cmds = Prelude.map (Data.Tuple.fst) (Prelude.map head parsedLines)
    let sts = (Prelude.map (readRepl st) (cmds))
    (mapM_ process cmds)
process (If c t e) = do
    st <- lift get 
    case eval (vars st) c of
        Just (IntVal 1) -> do
            process t
        Just (IntVal 0) -> do 
            process e
        Nothing -> do
            outputStrLn "Error: Conditional statement failed. Usage: If <condition> then <command> else <command>."

process (For cmd e cmd2 cmds) = do
    process cmd
    forHelper e cmd2 cmds
    
    
forHelper :: Expr -> Command -> [Command] -> InputT (StateT LState IO) ()
forHelper e cmd2 cmds = do 
    st <- lift get 
    case eval (vars st) e of
        Just (IntVal 1) -> do
            process cmd2
            mapM_ process cmds
            forHelper e cmd2 cmds
        Just (IntVal 0) -> do 
            return ()
        Nothing -> do
            outputStrLn "Output did not evaluate to a boolean."


readRepl :: LState -> Command -> LState
readRepl st (Set var expr) = case eval (vars st) expr of
    Just val -> LState $ updateVars var val (vars st)
    Nothing -> st
readRepl st (Print expr) = case eval (vars st) expr of
    Just val -> st
    Nothing -> st

-- walkthrough :: [LState] -> [Command] -> IO ()
-- walkthrough [] []    = pure ()
-- walkthrough (x:xs) (y:ys) = do process x y

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputT (StateT LState IO) ()
repl = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return ()  -- Exit on Ctrl+D or equivalent
    Just "quit" -> return ()
    Just input -> case parse pCommand input of
      [(cmd, "")] -> do
        -- 'process' now modifies the state directly
        process cmd
        repl  -- Repeat the loop with the potentially updated state
      _ -> do
        outputStrLn "Parse error"
        repl  -- Repeat the loop with the current state on parse error

completionFunction :: LState -> CompletionFunc IO
completionFunction st = completeWord Nothing " \t" $ return . findMatches (vars st)
    where
        findMatches varsList prefix = 
            Prelude.map simpleCompletion 
            $ Prelude.filter (prefix `isPrefixOf`)
            $ Data.HashMap.keys varsList

-- keysList :: Map Name Value -> [Name]
-- keysList map = Data.HashMap.keys map 

-- type StateData = [String]
-- replSettings :: Settings (StateT StateData IO) 
-- replSettings = Settings
--     { complete = completionFunction }