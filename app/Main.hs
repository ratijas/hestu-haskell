module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Environment

import Language.Hestu.Core

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> do
      let filename = args !! 0
      contents <- readFile filename
      runOne contents
    otherwise -> putStrLn "Program takes only 0 or 1 argument"
