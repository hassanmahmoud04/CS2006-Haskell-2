module Main where

import REPL (initLState, repl)
import System.Console.Haskeline
import Control.Monad.Trans.State (evalStateT)

main :: IO ()
main = runInputT defaultSettings $ evalStateT repl initLState
