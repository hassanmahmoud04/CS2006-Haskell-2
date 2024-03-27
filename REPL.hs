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

-- Uses two hashmaps in order to store variables and functions across each REPL loop
data LState = LState { vars :: Map Name Value, funcs :: Map Name [Command] }

-- initialises program state with both hashmaps empty
initLState :: LState
initLState = LState empty empty

-- Given a variable name and a value, return a new map of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> Map Name Value -> Map Name Value
updateVars name val env = insert name val (dropVar name env)



-- Return a new map of variables with the given name removed
dropVar :: Name -> Map Name Value -> Map Name Value
dropVar name env = delete name env

-- Updates funcion hashmap given a name and list of commands, overwriting old functions with the same name
updateFuncs :: Name -> [Command] -> Map Name [Command] -> Map Name [Command]
updateFuncs name cmds env = insert name cmds (dropFunc name env)

-- Returns a new hashmap with the given variable removed
dropFunc :: Name -> Map Name [Command] -> Map Name [Command]
dropFunc name env = delete name env


process :: Command -> InputT (StateT LState IO) ()
-- uses replicateM_ in order to carry out mapM_ n times over the list of commands
process (Repeat n cmds) = replicateM_ n (mapM_ process cmds) 
process (Set var Input) = do
    st <- lift get
    minput <- getInputLine ""  -- Directly read user input without prompting
    case minput of
        Nothing -> return () -- doesn't set variable if no input
        Just userInput -> do
            let val = StrVal(userInput)
            let updatedVars = updateVars var val (vars st)
            lift $ put st{ vars = updatedVars }
process (Set var expr) = do
    st <- lift get -- Lift the get operation from StateT into InputT (StateT IO LState)
    case eval (vars st) expr of
        Right val -> do
            let updatedVars = updateVars var val (vars st)
            lift $ put st{ vars = updatedVars }  -- Lift the put operation to update state
        Left str -> outputStrLn ("Error: " ++ str)
process (SetFunc name cmds) = do
    st <- lift get
    let updatedFuncs = updateFuncs name cmds (funcs st) -- adds new function to state hashmap
    lift $ put st{ funcs = updatedFuncs } -- updates state
process (RunFunc name) = do
    st <- lift get
    case funcEval (funcs st) name of -- checks if function exists
        Just cmds -> mapM_ process cmds -- if so, uses mapM_ to process each command in list and output results
        Nothing -> outputStrLn "Error: Evaluation failed."
process (Print expr) = do
    st <- lift get  -- Access the current state
    case eval (vars st) expr of
        Right val -> outputStrLn $ show val -- output value of evaluated expression
        Left str -> outputStrLn ("Error: " ++ str)
process (Read path) = do
    st <- lift get
    let concatPath = Prelude.filter (/='"') ("./" ++ show path ++ ".txt") -- removes unneccesary quotes
    outputStrLn ("Reading from file: " ++ (show concatPath))
    file <- lift ( lift(readFile (concatPath))) -- lifts filepath twice in order to be usable for StateT
    let allLines = lines file -- splits file string into list of lines
    let parsedLines = Prelude.map (parse pCommand) (allLines) -- parses each line using map
    let cmds = Prelude.map (Data.Tuple.fst) (Prelude.map head parsedLines) -- extracts the list of commands from the parsed lines
    (mapM_ process cmds) -- processes each command
process (If c t e) = do
    st <- lift get 
    case eval (vars st) c of -- checks if condition is fulfilled
        Right (IntVal 1) -> do
            process t -- then commands
        Right (IntVal 0) -> do 
            process e -- else commands
        Left str -> do
            outputStrLn "Error: Conditional statement failed. Usage: If <condition> then <command> else <command>."
process (For cmd e cmd2 cmds) = do -- for loop
    process cmd
    forHelper e cmd2 cmds
    
-- forHelper uses similar logic to the if else statement, recursively processing the commands and calling itself until the condition is false
forHelper :: Expr -> Command -> [Command] -> InputT (StateT LState IO) ()
forHelper e cmd2 cmds = do 
    st <- lift get 
    case eval (vars st) e of
        Right (IntVal 1) -> do
            process cmd2
            mapM_ process cmds
            forHelper e cmd2 cmds
        Right (IntVal 0) -> do 
            return ()
        Left str -> do
            outputStrLn ("Output did not evaluate to a boolean." ++ str)


-- Read, Eval, Print Loop
-- Updated to use StateT and Haskeline
-- Quit command no longer in Expr.hs, now included in REPL

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

-- Completion function to complete lines on use of TAB
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