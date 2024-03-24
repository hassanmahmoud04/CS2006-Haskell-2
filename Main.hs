module Main where

import System.Console.Haskeline (runInputT, defaultSettings)
import Parsing
import Expr
import REPL

main :: IO ()
main = runInputT defaultSettings (repl initLState)
