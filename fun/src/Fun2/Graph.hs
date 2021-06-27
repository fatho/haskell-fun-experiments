{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

module Fun2.Graph where

import Control.Lens (Ixed (ix), makeLenses, uses, view, (%=), (+=), (.=), (<<%=), use, (<<.=), at)
import Control.Monad (when)
import Control.Monad.State.Strict (evalState, execState, gets, runState, state)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable as Hashable
import qualified Data.Primitive as P
import qualified Data.Vector.Primitive as VP


import Debug.Trace

data Graph n = Graph
  { _freshRef :: !Ref
   -- TODO: invariant: make sure children always point to set representants
   -- when unioning two sets, replace all existing nodes using them
  , _interner :: !(HashMap (Node n) Ref)
  , _nodes    :: !(HashMap Ref (NodeData n))
  }
  deriving (Show, Eq)

data NodeData n = NodeData
  { _nodeSelf :: Node n
  , _nodeParents :: !(HashSet Ref)

  -- Union-Find stuff
  , _nodeClassParent :: Ref
  , _nodeClassRank   :: Rank
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

newtype Class = Class Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable, VP.Prim) -- deriving Prim requires UnboxedTuples and TypeInType

newtype Rank = Rank Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Bounded, Enum, Num, Hashable)


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

mkNodeData :: Ref -> Node n -> NodeData n
mkNodeData ref node = NodeData
  { _nodeSelf = node
  , _nodeParents = mempty
  -- Add a new union-find set
  , _nodeClassParent = ref
  , _nodeClassRank = 0
  }


makeLenses ''Graph
makeLenses ''NodeData

empty :: Graph n
empty = Graph
  { _freshRef = Ref 0
  , _interner = HashMap.empty
  , _nodes = HashMap.empty
  }

insert :: (Eq n, Hashable n) => Node n -> Graph n -> (Ref, Graph n)
insert (Node ty children) = runState $ do
  children' <- gets $ \g -> VP.map (`find` g) children
  let node = Node ty children'
  interner `uses` HashMap.lookup node >>= \case
    Nothing -> do
      ref <- freshRef <<%= succRef
      interner %= HashMap.insert node ref
      nodes %= HashMap.insert ref (mkNodeData ref node)
      -- Add back-edges from children to parent
      VP.forM_ children' $ \child ->
        nodes . ix child . nodeParents %= HashSet.insert ref
      pure ref
    Just ref -> pure ref


findWithRank :: Ref -> Graph n -> (Ref, Rank)
findWithRank start = evalState (go start)
  where
    go ref = do
      nodes `uses` HashMap.lookup ref >>= \case
        Nothing -> error $ "Node not part of this graph " ++ show ref
        Just nodeData
          | parent == ref -> pure (ref, view nodeClassRank nodeData)
          | otherwise -> go parent
          where
            parent = view nodeClassParent nodeData

find :: Ref -> Graph n -> Ref
find start = fst . findWithRank start

union :: (Eq n, Hashable n) => Ref -> Ref -> Graph n -> Graph n
union ref1 ref2 = execState $ do
  (class1, rank1) <- gets (findWithRank ref1)
  (class2, rank2) <- gets (findWithRank ref2)

  if rank1 < rank2
    then do
      -- make class2 parent of class1
      nodes . ix class1 . nodeClassParent .= class2
      fixupNodes class1 class2
    else do
      -- otherwise, put class2 into class1
      nodes . ix class2 . nodeClassParent .= class1
      fixupNodes class2 class1

      when (rank1 == rank2) $ do
        nodes . ix class1 . nodeClassRank += 1

  where
    -- fixup existing nodes
    fixupNodes from to = do
      --traceShowM ("fixupNodes", from, to)
      -- Get the nodes pointing to the "disappearing" node and unset parent pointers
      parents <- nodes . ix from . nodeParents <<.= mempty
      -- Rewrite each parent
      for_ parents $ \parent -> redirectChildren parent from to

    redirectChildren parent from to = do
      use (nodes . at parent) >>= \case
        Nothing -> error $ "Invariant violated: missing parent node " ++ show parent
        Just parentData -> do
          let
            oldNode@(Node ty oldChildren) = view nodeSelf parentData
            newChildren = VP.map (\child -> if child == from then to else child) oldChildren
            newNode = Node ty newChildren

          use (interner . at newNode) >>= \case
            -- The rewritten node has never been seen before,
            -- we can just pretend its always been this way
            Nothing -> do
              interner . at oldNode .= Nothing
              interner . at newNode .= Just parent
            -- The rewritten node already exists under a different ref,
            -- we need to merge the two now
            Just existingRef -> do
              interner . at oldNode .= Nothing
              nodes . at parent .= Nothing
              fixupNodes parent existingRef
