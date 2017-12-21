module Eval where

import Prelude hiding (last)
import Control.Monad.Error

import Data


eval :: SchleemVal -> ThrowsError SchleemVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, consequent, alternate]) = do
  result <- eval predicate
  case result of
    Bool False -> eval alternate
    _ -> eval consequent
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm


apply :: String -> [SchleemVal] -> ThrowsError SchleemVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognised primitive function args" func) ($ args) (lookup func primitives)


car :: [SchleemVal] -> ThrowsError SchleemVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList


cdr :: [SchleemVal] -> ThrowsError SchleemVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList


cons :: [SchleemVal] -> ThrowsError SchleemVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


primitives :: [(String, [SchleemVal] -> ThrowsError SchleemVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  ]


numericBinop :: (Integer -> Integer -> Integer) -> [SchleemVal] -> ThrowsError SchleemVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op args = (Number . foldl1 op) <$> mapM unpackNum args


numBoolBinop :: (Integer -> Integer -> Bool) -> [SchleemVal] -> ThrowsError SchleemVal
numBoolBinop  = boolBinop unpackNum


strBoolBinop :: (String -> String -> Bool) -> [SchleemVal] -> ThrowsError SchleemVal
strBoolBinop  = boolBinop unpackStr


boolBoolBinop :: (Bool -> Bool -> Bool) -> [SchleemVal] -> ThrowsError SchleemVal
boolBoolBinop = boolBinop unpackBool


boolBinop :: (SchleemVal -> ThrowsError a) -> (a -> a -> Bool) -> [SchleemVal] -> ThrowsError SchleemVal
boolBinop unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left <- unpacker $ head args
    right <- unpacker $ args !! 1
    return $ Bool $ left `op` right


unpackNum :: SchleemVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


unpackStr :: SchleemVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b) = return $ show b
unpackStr notStr = throwError $ TypeMismatch "string" notStr


unpackBool :: SchleemVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool
