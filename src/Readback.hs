{-# language OverloadedStrings #-}
module Readback where

import Protolude hiding (Seq, force, evaluate)

import Data.IORef

import qualified Domain
import qualified Evaluation
import Index
import Monad
import Sequence (Seq)
import qualified Sequence as Seq
import qualified Syntax
import qualified Tsil
import Var

-------------------------------------------------------------------------------
-- Readback environments

data Environment v = Environment
  { nextVar :: !(IORef Var)
  , vars :: Seq Var
  }

extend
  :: Environment v
  -> IO (Environment (Succ v), Var)
extend env = do
  var@(Var v) <- readIORef (nextVar env)
  writeIORef (nextVar env) (Var (v + 1))
  pure
    ( env
      { vars = vars env Seq.:> var
      }
    , var
    )

lookupIndex :: Var -> Environment v -> Index v
lookupIndex var context =
  fromMaybe (panic "Readback.lookupIndex") (lookupMaybeIndex var context)

lookupMaybeIndex :: Var -> Environment v -> Maybe (Index v)
lookupMaybeIndex var context =
  case Seq.elemIndex var (vars context) of
    Nothing ->
      Nothing

    Just i ->
      Just (Index (Seq.length (vars context) - i - 1))

-------------------------------------------------------------------------------

readback :: Environment v -> Domain.Value -> M (Syntax.Term v)
readback env value =
  case value of
    Domain.Neutral hd spine ->
      readbackNeutral env hd spine

    Domain.Lam name type_ closure -> do
      type' <- force type_
      Syntax.Lam name <$> readback env type' <*> readbackClosure env closure

    Domain.Pi name type_ closure -> do
      type' <- force type_
      Syntax.Pi name <$> readback env type' <*> readbackClosure env closure

    Domain.Fun source domain -> do
      source' <- force source
      domain' <- force domain
      Syntax.Fun <$> readback env source' <*> readback env domain'

readbackClosure :: Environment v -> Domain.Closure -> M (Scope Syntax.Term v)
readbackClosure env closure = do
  (env', v) <- extend env

  closure' <- Evaluation.evaluateClosure closure $ Lazy $ pure $ Domain.var v
  readback env' closure'

readbackNeutral :: Environment v -> Domain.Head -> Domain.Spine -> M (Syntax.Term v)
readbackNeutral env hd spine =
  case spine of
    Tsil.Nil ->
      pure $ readbackHead env hd

    Tsil.Snoc spine' arg -> do
      arg' <- force arg
      Syntax.App <$> readbackNeutral env hd spine' <*> readback env arg'

readbackHead :: Environment v -> Domain.Head -> Syntax.Term v
readbackHead env hd =
  case hd of
    Domain.Var v ->
      Syntax.Var $ lookupIndex v env

    Domain.Meta m ->
      Syntax.Meta m

    Domain.Global g ->
      Syntax.Global g