{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Fun2.Graph where

import Control.Lens (makeLenses, (%=), uses, (<<%=))
import Control.Monad.State.Strict (runState)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable as Hashable
import qualified Data.Primitive as P
import qualified Data.Vector.Primitive as VP


data Graph n = Graph
  { _freshRef :: !Ref
  , _interner :: !(HashMap (Node n) Ref)
  , _nodes :: !(HashMap Ref (Node n))
  }
  deriving (Show, Eq)

data Node n = Node !n !(VP.Vector Ref)
  deriving stock (Show, Eq, Generic)

instance Hashable n => Hashable (Node n) where
  hashWithSalt salt (Node ty children) =
    (salt `Hashable.hashWithSalt` ty) `hashPrimVectorWithSalt` children

newtype Ref = Ref Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable, VP.Prim) -- deriving Prim requires UnboxedTuples and TypeInType

succRef :: Ref -> Ref
succRef (Ref n) = Ref (n + 1)

hashPrimVectorWithSalt :: forall a. VP.Prim a => Int -> VP.Vector a -> Int
hashPrimVectorWithSalt salt (VP.Vector offset len (P.ByteArray arr#)) =
  let
    elemSize = P.sizeOf (undefined :: a)
    byteOff = offset * elemSize
    byteLen = len * elemSize
  in
    Hashable.hashByteArrayWithSalt arr# byteOff byteLen salt

makeLenses ''Graph

empty :: Graph n
empty = Graph
  { _freshRef = Ref 0
  , _interner = HashMap.empty
  , _nodes = HashMap.empty
  }

insert :: (Eq n, Hashable n) => Node n -> Graph n -> (Ref, Graph n)
insert node = runState $ do
  interner `uses` HashMap.lookup node >>= \case
    Nothing -> do
      ref <- freshRef <<%= succRef
      interner %= HashMap.insert node ref
      nodes %= HashMap.insert ref node
      pure ref
    Just ref -> pure ref
