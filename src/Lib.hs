module Lib
    ( schleem
    ) where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


data SchleemVal = Atom String
                | List [SchleemVal]
                | DottedList [SchleemVal] SchleemVal
                | Number Integer
                | String String
                | Bool Bool



schleem :: IO ()
schleem = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr


readExpr :: String -> String
readExpr input =
  case parse (spaces >> symbol) "schleem" input of
    Right val -> "Found value: " ++ show val
    Left err -> "No match: " ++ show err


parseString :: Parser SchleemVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x


parseAtom :: Parser SchleemVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space
