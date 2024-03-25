module Main where

import REPL (initLState, repl)
import System.Console.Haskeline
import Control.Monad.Trans.State (execStateT)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = runInputT defaultSettings $ liftIO (execStateT repl initLState >> return ())
