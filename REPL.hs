module REPL where

import System.Console.Haskeline
import Control.Monad
import Control.Monad (foldM)
import Control.Monad (replicateM_)
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

process :: LState -> Command -> InputT IO LState
process st (Repeat n cmds) =
    foldM (\accSt _ -> foldM process accSt cmds) st [1..n]
process st (Set var expr) = do
    case eval (vars st) expr of
        Just val -> do
            let updatedVars = updateVars var val (vars st)
            let st' = st { vars = updatedVars }
            return st'
        Nothing -> do
            outputStrLn "Error: Evaluation failed."
            return st
process st (Print expr) = do
    case eval (vars st) expr of
        Just val -> do
            outputStrLn $ show val
            return st
        Nothing -> do
            outputStrLn "Error: Evaluation failed."
            return st
process st Quit = return st




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
        newState <- process st cmd -- process returns updated state
        repl newState -- Continue with the updated state
      _ -> do
        outputStrLn "Parse error"
        repl st -- Continue with the current state on parse error




completionFunction :: LState -> CompletionFunc IO
completionFunction st = completeWord Nothing " \t" $ return . findMatches (vars st)
  where
    findMatches varsList prefix = 
      map simpleCompletion 
      $ filter (prefix `isPrefixOf`) 
      $ map fst varsList
