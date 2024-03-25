module Main where

import REPL
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings (repl initLState)
