import Control.Monad.Trans.State (runStateT)
import System.Console.Haskeline (runInputT, defaultSettings)
import REPL (repl, initLState)
import Control.Monad

main :: IO ()
main = void $ runStateT (runInputT defaultSettings repl) initLState