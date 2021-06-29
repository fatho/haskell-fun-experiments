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

module Fun2.Graph
  ( Graph
  , Ref
  , Node (..)
  , empty
  , insert
  , classOf
  , classNodes
  , equalize
  , classes
  , dot
  ) where

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
import qualified Data.Vector.Primitive as VP

import Fun2.UnionFind (UnionFind)
import qualified Fun2.UnionFind as UF
import Fun2.Util (hashPrimVectorWithSalt)

-- | A term graph supporting equivalence relations.
data Graph n = Graph
  { _graphFreshRef  :: !Ref
    -- ^ Used for generating fresh node IDs when new nodes are inserted

  , _graphInterner  :: !(HashMap (Node n) Ref)
    -- ^ Used for hash-consing incoming nodes
    -- Invariant: all refs on the RHS of the map exist in '_graphNodes'

  , _graphNodes     :: !(HashMap Ref (NodeData n))
    -- ^ Nodes in the graph together with some metadata

  , _graphUnionFind :: !(UnionFind Ref)
    -- ^ Union-find structure for recording the equivalence relation

  , _graphClasses   :: !(HashMap Ref ClassData)
    -- ^ Meta-data about the equivalence classes
    -- Invariant: the keys in this map are exactly the class representatives
  }
  deriving (Show, Eq)

-- | Information stored about a single node.
data NodeData n = NodeData
  { _nodeDataSelf    :: !(Node n)
    -- ^ The actual node
  , _nodeDataParents :: !(HashSet Ref)
    -- ^ The set of nodes pointing to this node
  }
  deriving (Show, Eq)

-- | Information about an equivalence class.
--
-- Forms a commutative monoid. This is important because class info needs to be merged when classes
-- are equalized, and equalization can happen in any order.
data ClassData = ClassData
  { _classDataNodes   :: !(HashSet Ref)
    -- ^ The nodes that are part of this class.
  , _classDataParents :: !(HashSet Ref)
    -- ^ The set of all nodes that have children in this class.
  }
  deriving (Show, Eq)

-- | A node, characterized by its type @n@ and an ordered list of children.
data Node n = Node !n !(VP.Vector Ref)
  deriving stock (Show, Eq, Generic)

instance Hashable n => Hashable (Node n) where
  hashWithSalt salt (Node ty children) =
    (salt `Hashable.hashWithSalt` ty) `hashPrimVectorWithSalt` children

newtype Ref = Ref Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, VP.Prim) -- deriving Prim requires UnboxedTuples and TypeInType

succRef :: Ref -> Ref
succRef (Ref n) = Ref (n + 1)

makeLenses ''Graph
makeLenses ''NodeData
makeLenses ''ClassData

instance Semigroup ClassData where
  cd1 <> cd2 = ClassData
    { _classDataNodes = _classDataNodes cd1 <> _classDataNodes cd2
    , _classDataParents = _classDataParents cd1 <> _classDataParents cd2
    }

instance Monoid ClassData where
  mempty = ClassData mempty mempty

-- | An empty graph.
empty :: Graph n
empty = Graph
  { _graphFreshRef = Ref 0
  , _graphInterner = HashMap.empty
  , _graphNodes = HashMap.empty
  , _graphUnionFind = UF.empty
  , _graphClasses = HashMap.empty
  }

-- | Insert a new node into the graph and return its id and the modified graph.
--
-- TODO: ensure that new nodes are inserted into the correct equivalence class if an equivalent node
-- already exists (by congruence, as exactly identical nodes are already interned).
insert :: (Eq n, Hashable n) => Node n -> Graph n -> (Ref, Graph n)
insert node@(Node _ children) = runState $ do
  graphInterner `uses` HashMap.lookup node >>= \case
    Nothing -> do
      ref <- graphFreshRef <<%= succRef
      graphInterner %= HashMap.insert node ref
      graphNodes %= HashMap.insert ref (NodeData node mempty)
      graphUnionFind %= UF.insert ref
      graphClasses %= HashMap.insert ref (ClassData (HashSet.singleton ref) mempty)
      -- Add back-edges from children to parent
      VP.forM_ children $ \child -> do
        graphNodes . ix child . nodeDataParents %= HashSet.insert ref
        childClass <- gets $ classOf child
        graphClasses . ix childClass . classDataParents %= HashSet.insert ref
      pure ref
    Just ref -> pure ref

-- | Get the class-representing node of an arbitrary node in the graph.
classOf :: Ref -> Graph n -> Ref
classOf ref = UF.find ref . _graphUnionFind

-- | Return the nodes that are part of the same equivalence class as the given node.
classNodes :: Ref -> Graph n -> HashSet Ref
classNodes ref g = view (graphClasses . ix rep . classDataNodes) g
  where
    rep = classOf ref g

-- | Return a list of all classes with all their respective nodes.
classes :: Graph n -> [[(Ref, Node n)]]
classes g
  = map (sortBy (comparing fst) . snd)
  $ Map.toList nodesByClass
  where
    nodesByClass = Map.fromListWith (++)
      [ (classRef, [(ref, view nodeDataSelf nd)])
      | (ref, nd) <- HashMap.toList $ view graphNodes g
      , let classRef = classOf ref g
      ]

-- | @equalize a b g@ records the fact that @a@ is equivalent to @b@ by merging their corresponding
-- equivalence classes in @g@.
equalize :: (Eq n, Hashable n) => Ref -> Ref -> Graph n -> Graph n
equalize ref1 ref2 = execState $
  zoom graphUnionFind (state $ UF.union' ref1 ref2) >>= \case
    -- Already equal
    Nothing -> pure ()
    Just unioned -> do
      -- Update class info
      graphClasses . at (UF.unionAbsorbed unioned) <<.= Nothing >>= \case
        Nothing           -> error "invariant violated: class representant has no ClassData"
        Just absorbedData -> graphClasses . ix (UF.unionInto unioned) <>= absorbedData
      -- Exploit congruence (@x ≡ y@ implies @f x ≡ f y@) to merge parents
      propagateUnions $ UF.unionInto unioned
  where
    -- Unioning two nodes could mean that two disjoint nodes referencing them now become equal
    -- as well. For example, suppose we have the expressions @x + y@ and @x + 1@.
    -- If it now turns out that @y@ ≡ @1@, then we also have @x + y@ ≡ @x + 1@ by substitution.
    propagateUnions newClassRep = do
      -- Obtain all nodes pointing to our newly formed class
      parents <- uses (graphClasses . ix newClassRep . classDataParents) HashSet.toList
      -- Compute which parents now become equivalent due to their children being merged
      let
        computeParentUnions _ [] acc = pure acc
        computeParentUnions seen (parentRef:rest) acc = use (graphNodes . at parentRef) >>= \case
            Nothing -> error $ "Invariant violated: missing node " ++ show parentRef
            Just nd -> do
              let Node ty children = view nodeDataSelf nd
              classChildren <- gets $ \g -> VP.map (`classOf` g) children
              let normalNode = Node ty classChildren
              case HashMap.lookup normalNode seen of
                Nothing -> computeParentUnions (HashMap.insert normalNode parentRef seen) rest acc
                Just existing -> computeParentUnions seen rest ((parentRef, existing):acc)

      congruences <- computeParentUnions HashMap.empty parents []
      -- Merge those parents in turn
      for_ congruences $ \(p1, p2) -> id %= equalize p1 p2


-- | Generate a GraphViz DOT representation of the equality-saturation graph.
dot :: (Show n, Eq n, Hashable n) => Graph n -> String
dot g = unlines $ header ++ clusterLines ++ edgeLines ++ footer
  where
    header =
        [ "digraph ast {"
        , "  compound=true;"
        ]
    clusterLines = concatMap (map (indent 2) . mkCluster) $ HashMap.toList clusters
    edgeLines = concatMap (map (indent 2) . mkEdges) allNodes
    footer = ["}"]

    clusters = HashMap.fromListWith (++)
      [ (repr, [(ref, nodeData)])
      | (ref, nodeData) <- allNodes
      , let repr = classOf ref g
      ]

    allNodes = HashMap.toList $ view graphNodes g

    indent i = (replicate i ' ' ++)

    mkCluster (Ref reprId, clusterNodes) =
      let
        clusterHeader = [ "subgraph cluster_" ++ show reprId ++ " {" ]
        nodeLines = concatMap (map (indent 2) . mkNode) clusterNodes
        clusterFooter = ["}"]
      in
        clusterHeader ++ nodeLines ++ clusterFooter

    mkNode (ref, nodeData) =
      let Node label _ = view nodeDataSelf nodeData
       in [mkNodeId ref ++ "[label=" ++ show label ++ "];"]

    mkEdges (ref, nodeData) =
      let Node _ children = view nodeDataSelf nodeData
       in [ edgeLine
          | (index, target) <- zip [0 :: Int ..] $ VP.toList children
          , edgeLine <- mkEdge index ref target
          ]

    mkEdge index from to
      -- If the node points to the same cluster, we need an invisible extra node
      | toRepr == classOf from g =
          let
            helperNode = "helper_" ++ show index ++ "_" ++ mkNodeId from ++ "_" ++ mkNodeId to
          in
            [ helperNode ++ "[shape=point height=0];" -- invisible
            , mkNodeId to ++ " -> " ++ helperNode ++ "[dir=back ltail=cluster_" ++ show toReprId ++ "];"
            , helperNode ++ " -> " ++ mkNodeId from ++ "[dir=none, arrowhead=none];"
            ]
      | otherwise =
          [ mkNodeId from ++ " -> " ++ mkNodeId to ++ " [lhead=cluster_" ++ show toReprId ++ "]" ]
      where
        toRepr@(Ref toReprId) = classOf to g


    mkNodeId (Ref ref) = "node_" ++ show ref
