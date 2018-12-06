module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Environment

import Language.Hestu.Core

-- main :: IO ()
-- main = do args <- getArgs
--           if null args
--             then runRepl
--             else runOne $ args


main :: IO ()
main = do args <- liftIO $ getArgs
          let script = args !! 0
          evaled <- return $ liftM show $ readBody script >>= execBody
          putStrLn $ extractValue $ trapError evaled
