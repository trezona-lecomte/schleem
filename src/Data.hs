module Data where

import Control.Monad.Error
import Prelude hiding (head, tail)

import Text.ParserCombinators.Parsec (ParseError)


-- SchleemVal ------------------------------------------------------------------

data SchleemVal = Atom String
                | List [SchleemVal]
                | DottedList [SchleemVal] SchleemVal
                | Number Integer
                | String String
                | Bool Bool


instance Show SchleemVal where
  show = showVal


showVal :: SchleemVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


unwordsList :: [SchleemVal] -> String
unwordsList = unwords . map showVal


-- SchleemError ----------------------------------------------------------------

data SchleemError = NumArgs Integer [SchleemVal]
                  | TypeMismatch String SchleemVal
                  | Parser ParseError
                  | BadSpecialForm String SchleemVal
                  | NotFunction String String
                  | UnboundVar String String
                  | Default String


instance Show SchleemError where
  show = showError


instance Error SchleemError where
  noMsg = Default "An error has occurred"
  strMsg = Default


showError :: SchleemError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values: " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ show expected ++ ", found: " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varName) = message ++ ": " ++ varName
showError (Default message) = message

-- showError (Default message)
