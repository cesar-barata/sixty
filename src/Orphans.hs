{-# language FlexibleContexts #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans #-}
module Orphans where

import Protolude hiding (IntMap, get, put)

import Data.Dependent.HashMap (DHashMap)
import qualified Data.Dependent.HashMap as DHashMap
import Data.GADT.Compare (GEq)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Constraint.Extras
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IntervalMap.FingerTree (IntervalMap)
import qualified Data.IntervalMap.FingerTree as IntervalMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Persist
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import Rock.Traces

instance (Persist k, Persist v, Coercible Int k) => Persist (IntMap k v) where
  put =
    put . IntMap.toList

  get =
    IntMap.fromList <$> get

instance (Persist k, Eq k, Hashable k, Persist v) => Persist (HashMap k v) where
  put =
    put . HashMap.toList

  get =
    HashMap.fromList <$> get

instance (Persist a, Eq a, Hashable a) => Persist (HashSet a) where
  put =
    put . HashSet.toList

  get =
    HashSet.fromList <$> get

instance Persist a => Persist (Tsil a) where
  put =
    put . toList

  get =
    Tsil.fromList <$> get

instance Persist k => Persist (IntervalMap.Interval k) where
  put (IntervalMap.Interval a b) =
    put (a, b)

  get =
    uncurry IntervalMap.Interval <$> get

instance (Persist k, Ord k, Persist v) => Persist (IntervalMap k v) where
  put m =
    put $ fold $
      (`IntervalMap.intersections` m)
      <$> IntervalMap.bounds m

  get =
    mconcat . map (uncurry IntervalMap.singleton) <$> get

instance Hashable k => Hashable (IntervalMap.Interval k) where
  hashWithSalt s (IntervalMap.Interval a b) =
    hashWithSalt s (a, b)

instance (Hashable k, Ord k, Hashable v) => Hashable (IntervalMap k v) where
  hashWithSalt s m =
    hashWithSalt s $
      (`IntervalMap.intersections` m)
      <$> IntervalMap.bounds m

instance (Persist v, GEq k, Hashable (DHashMap.Some k), Persist (DHashMap.Some k), Has' Persist k dep) => Persist (ValueDeps k dep v) where
  put (ValueDeps a b) = do
    put a
    put b

  get =
    ValueDeps <$> get <*> get

instance (Persist (DHashMap.Some k), Has' Persist k f, GEq k, Hashable (DHashMap.Some k)) => Persist (DHashMap k f) where
  put m = do
    put @Int $ DHashMap.size m
    forM_ (DHashMap.toList m) $ \(k DHashMap.:=> f) ->
      has' @Persist @f k (put f)

  get =
    DHashMap.fromList <$> do
      n <- get @Int
      replicateM n $ do
        DHashMap.Some k <- get
        f <- has' @Persist @f k get
        pure $ k DHashMap.:=> f

instance Persist a => Persist (Identity a) where
  put (Identity a) =
    put a

  get =
    Identity <$> get

instance Persist a => Persist (Const a b) where
  put (Const a) =
    put a

  get =
    Const <$> get
