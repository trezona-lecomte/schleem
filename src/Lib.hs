module Lib
    ( schleem
    ) where

import Control.Monad.Error
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)

import Eval (eval)
import Data
import Parser (parseExpr)


schleem :: IO ()
schleem = do
  args <- getArgs
  let evaluated = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaluated

readExpr :: String -> ThrowsError SchleemVal
readExpr input =
  case parse parseExpr "schleem" input of
    Right val -> return val
    Left err -> throwError $ Parser err
