module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Environment

import Language.Scheme.VM.REPL
import Language.Scheme.VM.Core

-- main :: IO ()
-- main = do args <- getArgs
--           if null args
--             then runRepl
--             else runOne $ args


main :: IO ()
main = do args <- liftIO $ getArgs
          let script = args !! 0
          (runIOThrows $ liftM show $ (liftThrows $ readDExpr script)) >>= putStrLn
