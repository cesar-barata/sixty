{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
module Monad where

import Protolude hiding (try, State)

import Control.Monad.Base
import Data.IORef.Lifted
import Control.Exception.Lifted
import Rock

import qualified Error
import Query (Query)
import Var

type M = ReaderT State (Task Query)

newtype State = State
  { nextVar :: IORef Var
  }

newtype Lazy a = Lazy { force :: M a }

lazy :: MonadBase IO m => M a -> m (Lazy a)
lazy m = do
  ref <- newIORef $ panic "Can't happen, I promise!"
  writeIORef ref $ do
    result <- m
    writeIORef ref $ pure result
    pure result
  pure $ Lazy $ join $ readIORef ref

eager :: a -> Lazy a
eager = Lazy . pure

freshVar :: M Var
freshVar = do
  ref <- asks nextVar
  atomicModifyIORef ref $ \var@(Var i) ->
    (Var $ i + 1, var)

runM :: M a -> Task Query (Either Error.Elaboration a)
runM r = do
  nextVarVar <- newIORef $ Var 0
  try $ runReaderT r State
    { nextVar = nextVarVar
    }

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM p (x:xs) = do
  b <- p x
  if b then
    allM p xs
  else
    return False

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM p (x:xs) = do
  b <- p x
  if b then
    return True
  else
    anyM p xs
