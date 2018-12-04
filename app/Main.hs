module Main where

import System.Environment

import Language.Scheme.VM.REPL
import Language.Scheme.VM.Core

main :: IO ()
main = do args <- getArgs
          if null args
            then runRepl
            else runOne $ args
