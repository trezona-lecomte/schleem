module Data where

data SchleemVal = Atom String
                | List [SchleemVal]
                | DottedList [SchleemVal] SchleemVal
                | Number Integer
                | String String
                | Bool Bool
                deriving (Show)
