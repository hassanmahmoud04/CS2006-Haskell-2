module REPL where

import System.Console.Haskeline
import Control.Monad
import Control.Monad (foldM)
import Control.Monad (replicateM_)
import Expr
import Parsing
import Control.Monad.IO.Class (liftIO) -- For liftIO
import Data.List (isPrefixOf)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)


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

process :: Command -> StateT LState IO ()
process (Repeat n cmds) = replicateM_ n $ mapM_ process cmds
process (Set var expr) = do
    st <- get -- Retrieve the current state
    case eval (vars st) expr of
        Just val -> do
            let updatedVars = updateVars var val (vars st)
            put st { vars = updatedVars } -- Update the state with the new variables
        Nothing -> liftIO $ putStrLn "Error: Evaluation failed."
process (Print expr) = do
    st <- get -- Retrieve the current state
    case eval (vars st) expr of
        Just val -> liftIO $ putStrLn $ show val -- Print the evaluated expression
        Nothing -> liftIO $ putStrLn "Error: Evaluation failed."
process Quit = return () -- Quit command does not change the state




-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: StateT LState IO ()
repl = do
  minput <- liftIO $ runInputT defaultSettings $ getInputLine "> "
  case minput of
    Nothing -> return ()  -- Exit on Ctrl+D
    Just input -> do
      -- Parse the command from the input
      case parse pCommand input of
        [(cmd, "")] -> do
          -- Process the command, potentially updating the state
          process cmd
          -- Recursively call repl to handle the next command
          repl
        _ -> do
          -- Handle parse errors
          liftIO $ putStrLn "Parse error"
          repl




completionFunction :: LState -> CompletionFunc IO
completionFunction st = completeWord Nothing " \t" $ return . findMatches (vars st)
  where
    findMatches varsList prefix = 
      map simpleCompletion 
      $ filter (prefix `isPrefixOf`) 
      $ map fst varsList