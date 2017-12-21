module Lib
    ( schleem
    ) where

import System.Environment (getArgs)

import REPL


schleem :: IO ()
schleem = do
  args <- getArgs
  case length args of
    0 -> repl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Pass zero args for REPL or a single arg to evaluate"
