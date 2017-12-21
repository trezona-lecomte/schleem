module REPL where

import Prelude hiding (until)
import Control.Monad.Error
import System.IO
import Text.ParserCombinators.Parsec (parse)

import Eval
import Data
import Parser
import State


repl :: IO ()
repl = until (== "quit") (readPrompt "Schleem>>> ") evalAndPrint


until :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until predicate prompt action = do
  result <- prompt
  unless (predicate result)
    (action result >> until predicate prompt action)


evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn


evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (show <$> (readExpr expr >>= eval))


readExpr :: String -> ThrowsError SchleemVal
readExpr input =
  case parse parseExpr "schleem" input of
    Right val -> return val
    Left err -> throwError $ Parser err


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
