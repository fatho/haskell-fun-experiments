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

import Control.Lens (Ixed (ix), at, makeLenses, use, uses, view, zoom, (%=), (<<%=), (<<.=), (<>=))
import Control.Monad.State.Strict (execState, gets, runState, state)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Hashable as Hashable
import qualified Data.Map.Strict as Map
import qualified Data.Primitive as P
import qualified Data.Vector.Primitive as VP

import Fun2.UnionFind (UnionFind)
import qualified Fun2.UnionFind as UF

-- | Invariant: nodes always point to set-representants
data Graph n = Graph
  { _freshRef  :: !Ref

  , _interner  :: !(HashMap (Node n) Ref)
  , _nodes     :: !(HashMap Ref (NodeData n))
  , _unionFind :: !(UnionFind Ref)
  , _classes   :: !(HashMap Ref ClassData)
  }
  deriving (Show, Eq)

data NodeData n = NodeData
  { _nodeSelf    :: !(Node n)
  , _nodeParents :: !(HashSet Ref)
  }
  deriving (Show, Eq)

data ClassData = ClassData
  { _classNodes   :: !(HashSet Ref)
  , _classParents :: !(HashSet Ref)
  }
  deriving (Show, Eq)

data Node n = Node !n !(VP.Vector Ref)
  deriving stock (Show, Eq, Generic)

instance Hashable n => Hashable (Node n) where
  hashWithSalt salt (Node ty children) =
    (salt `Hashable.hashWithSalt` ty) `hashPrimVectorWithSalt` children

newtype Ref = Ref Int
  deriving stock (Show, Eq, Ord, Generic)
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


makeLenses ''Graph
makeLenses ''NodeData
makeLenses ''ClassData

instance Semigroup ClassData where
  cd1 <> cd2 = ClassData
    { _classNodes = _classNodes cd1 <> _classNodes cd2
    , _classParents = _classParents cd1 <> _classParents cd2
    }

instance Monoid ClassData where
  mempty = ClassData mempty mempty

empty :: Graph n
empty = Graph
  { _freshRef = Ref 0
  , _interner = HashMap.empty
  , _nodes = HashMap.empty
  , _unionFind = UF.empty
  , _classes = HashMap.empty
  }

insert :: (Eq n, Hashable n) => Node n -> Graph n -> (Ref, Graph n)
insert (Node ty children) = runState $ do
  classChildren <- gets $ \g -> VP.map (`classOf` g) children
  let node = Node ty classChildren
  interner `uses` HashMap.lookup node >>= \case
    Nothing -> do
      ref <- freshRef <<%= succRef
      interner %= HashMap.insert node ref
      nodes %= HashMap.insert ref (NodeData node mempty)
      unionFind %= UF.insert ref
      classes %= HashMap.insert ref (ClassData (HashSet.singleton ref) mempty)
      -- Add back-edges from children to parent
      VP.forM_ classChildren $ \child -> do
        nodes . ix child . nodeParents %= HashSet.insert ref
        classes . ix child . classParents %= HashSet.insert ref
      pure ref
    Just ref -> pure ref


classOf :: Ref -> Graph n -> Ref
classOf start = UF.find start . _unionFind

nodeClasses :: Graph n -> [[(Ref, Node n)]]
nodeClasses g
  = map (sortBy (comparing fst) . snd)
  $ Map.toList nodesByClass
  where
    nodesByClass = Map.fromListWith (++)
      [ (classRef, [(ref, view nodeSelf nd)])
      | (ref, nd) <- HashMap.toList $ _nodes g
      , let classRef = classOf ref g
      ]

union :: (Eq n, Hashable n) => Ref -> Ref -> Graph n -> Graph n
union ref1 ref2 = execState $
  zoom unionFind (state $ UF.union ref1 ref2) >>= \case
    -- Already equal
    Nothing -> pure ()
    Just unioned -> do
      -- Update internal data structures
      absorbedData <- classes . at (UF.unionAbsorbed unioned) <<.= Nothing
      for_ absorbedData $ \cd -> classes . ix (UF.unionInto unioned) <>= cd
      propagateUnions $ UF.unionInto unioned
  where
    -- Unioning two nodes could mean that two disjoint nodes referencing them now become equal
    -- as well. For example, suppose we have the expressions @x + y@ and @x + 1@.
    -- If it now turns out that @y@ ≡ @1@, then we also have @x + y@ ≡ @x + 1@ by substitution.
    propagateUnions newClassRep = do
      -- Obtain all nodes pointing to our newly formed class
      parents <- uses (classes . ix newClassRep . classParents) HashSet.toList
      -- Compute which parents now become equivalent due to their children being merged
      let
        computeParentUnions _ [] acc = pure acc
        computeParentUnions seen (parentRef:rest) acc = use (nodes . at parentRef) >>= \case
            Nothing -> error $ "Invariant violated: missing node " ++ show parentRef
            Just nd -> do
              let Node ty children = view nodeSelf nd
              classChildren <- gets $ \g -> VP.map (`classOf` g) children
              let normalNode = Node ty classChildren
              case HashMap.lookup normalNode seen of
                Nothing -> computeParentUnions (HashMap.insert normalNode parentRef seen) rest acc
                Just existing -> computeParentUnions seen rest ((parentRef, existing):acc)

      congruences <- computeParentUnions HashMap.empty parents []
      -- Merge those parents in turn
      for_ congruences $ \(p1, p2) -> id %= union p1 p2
