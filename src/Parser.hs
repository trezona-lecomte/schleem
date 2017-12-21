module Parser where

import Prelude hiding (head, tail)

import Text.ParserCombinators.Parsec hiding (spaces)

import Data


parseExpr :: Parser SchleemVal
parseExpr = parseString
            <|> parseAtom
            <|> parseNumber
            <|> parseQuoted
            <|> do _ <- char '('
                   x <- try parseList <|> parseDottedList
                   _ <- char ')'
                   return x


parseString :: Parser SchleemVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
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
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space
