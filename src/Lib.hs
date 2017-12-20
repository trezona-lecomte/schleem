module Lib
    ( schleem
    ) where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

schleem :: IO ()
schleem = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr


readExpr :: String -> String
readExpr input =
  case parse (spaces >> symbol) "schleem" input of
    Right val -> "Found value: " ++ show val
    Left err -> "No match: " ++ show err

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space
