module Eval where

import Control.Monad.Error

import Data


eval :: SchleemVal -> ThrowsError SchleemVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm


apply :: String -> [SchleemVal] -> ThrowsError SchleemVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognised primitive function args" func) ($ args) (lookup func primitives)


primitives :: [(String, [SchleemVal] -> ThrowsError SchleemVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  ]


numericBinop :: (Integer -> Integer -> Integer) -> [SchleemVal] -> ThrowsError SchleemVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op args = (Number . foldl1 op) <$> mapM unpackNum args


unpackNum :: SchleemVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
