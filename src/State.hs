module State where

import Control.Monad.Error
import Data.Maybe (isJust)
import Data.IORef

import Data


-- We need IORefs for both the list itself and for individual values because there are two ways that the program can mutate the environment:
-- It might use set! to change the value of an individual variable, a change visible to any function that shares that environment (Scheme allows nested scopes, so a variable in an outer scope is visible to all inner scopes).
-- Or it might use define to add a new variable, which should be visible on all subsequent statements.

type Env = IORef [(String, IORef SchleemVal)]


type IOThrowsError = ErrorT SchleemError IO


type ThrowsError = Either SchleemError


nullEnv :: IO Env
nullEnv = newIORef []


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue


trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "extractValue must be used only after catchError"


-- Environment handling

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef


getVar :: Env -> String -> IOThrowsError SchleemVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  case lookup var env of
    Nothing -> throwError $ UnboundVar "Attempted to get an unbound variable " var
    Just var' -> (liftIO . readIORef) var'


setVar :: Env -> String -> SchleemVal -> IOThrowsError SchleemVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Attempted to set an unbound variable" var)
    (liftIO . (`writeIORef` val))
    (lookup var env)
  return val


defineVar :: Env -> String -> SchleemVal -> IOThrowsError SchleemVal
defineVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
  then setVar envRef var val >> return val
  else liftIO $ do
    valRef <- newIORef val
    env <- readIORef envRef
    writeIORef envRef ((var, valRef) : env)
    return val
