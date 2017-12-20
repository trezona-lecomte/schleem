module Lib
    ( schleem
    ) where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)

import Data
import Parser (parseExpr)


schleem :: IO ()
schleem = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr


readExpr :: String -> String
readExpr input =
  case parse parseExpr "schleem" input of
    Right val -> "Found value: " ++ show val
    Left err -> "No match: " ++ show err
