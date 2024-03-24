module REPL where

import System.Console.Haskeline
import Expr
import Parsing
import Control.Monad.IO.Class (liftIO) -- For liftIO
import Data.List (isPrefixOf)

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



process :: LState -> Command -> InputT IO ()
process st (Set var Input) = do
    minput <- getInputLine ""  -- Prompt directly for user input
    case minput of
        Nothing -> outputStrLn "No input provided."
        Just userInput -> do
            let val = StrVal userInput  -- Treat input as a string value
            let st' = LState $ updateVars var val (vars st)
            repl st'
process st (Set var expr) = do
    case eval (vars st) expr of
        Just val -> do
            let st' = LState $ updateVars var val (vars st)
            repl st'
        Nothing -> outputStrLn "Error: Evaluation failed."
process st (Print expr) = do
    case eval (vars st) expr of
        Just val -> do
            outputStrLn $ show val
            repl st
        Nothing -> outputStrLn "Error: Evaluation failed."
process st Quit = outputStrLn "Exiting code..."




-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> InputT IO ()
repl st = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return () -- Exit on Ctrl+D
    Just input -> case parse pCommand input of
      [(cmd, "")] -> do
        process st cmd
        repl st -- This now correctly stays within InputT IO monad
      _ -> do
        outputStrLn "Parse error"
        repl st



completionFunction :: LState -> CompletionFunc IO
completionFunction st = completeWord Nothing " \t" $ return . findMatches (vars st)
  where
    findMatches varsList prefix = 
      map simpleCompletion 
      $ filter (prefix `isPrefixOf`) 
      $ map fst varsList