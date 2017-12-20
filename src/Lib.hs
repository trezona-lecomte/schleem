module Lib
    ( schleem
    ) where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)


data SchleemVal = Atom String
                | List [SchleemVal]
                | DottedList [SchleemVal] SchleemVal
                | Number Integer
                | String String
                | Bool Bool
                deriving (Show)



schleem :: IO ()
schleem = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr


readExpr :: String -> String
readExpr input =
  case parse parseExpr "schleem" input of
    Right val -> "Found value: " ++ show val
    Left err -> "No match: " ++ show err


parseExpr :: Parser SchleemVal
parseExpr = parseString
            <|> parseAtom
            <|> parseNumber
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x


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


parseNumber :: Parser SchleemVal
parseNumber = (Number . read) <$> many1 digit


parseList :: Parser SchleemVal
parseList = List <$> parseExpr `sepBy` spaces


parseDottedList :: Parser SchleemVal
parseDottedList = do
  head <- parseExpr `endBy` spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail


parseQuoted :: Parser SchleemVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space
