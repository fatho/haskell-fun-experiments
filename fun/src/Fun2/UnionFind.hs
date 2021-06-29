-- | Implementation of union-find using arbitrary hashable ids.
module Fun2.UnionFind
  ( UnionFind
  , empty
  , insert
  , find
  , union
  , Union (..)
  , sets
  )
  where

import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (sortOn)

-- | A rank-based union-find/disjoint-set data structure.
newtype UnionFind k = UnionFind (HashMap k (Set k))

-- | Not very efficient, intended to be used in the tests.
instance (Eq k, Hashable k) => Eq (UnionFind k) where
  UnionFind sets1 == UnionFind sets2 = byClass sets1 == byClass sets2
    where
      byClass hm
        = HashSet.fromList
        $ HashMap.elems
        $ HashMap.fromListWith HashSet.union
          [ (rep, HashSet.singleton setElem)
          | setElem <- HashMap.keys hm
          , let rep = find setElem (UnionFind hm)
          ]

instance (Show k, Eq k, Hashable k) => Show (UnionFind k) where
  show (UnionFind hm)
    = show
    $ sortOn show
    $ map (sortOn show)
    $ HashMap.elems
    $ HashMap.fromListWith (++)
      [ (rep, [setElem])
      | setElem <- HashMap.keys hm
      , let rep = find setElem (UnionFind hm)
      ]

-- | Internal data about a union-find set.
data Set k = Set
  { setParent :: !k
  , setRank :: !Int
  }

-- | An empty union-find data structure.
empty :: UnionFind k
empty = UnionFind HashMap.empty

-- | Insert a new set into the union-find datastructure.
insert :: (Eq k, Hashable k) => k -> UnionFind k -> UnionFind k
insert key (UnionFind hm) = UnionFind $ HashMap.insertWith (\_new old -> old) key (Set key 0) hm

-- | Find the key representing the set that the given key is part of.
find :: (Eq k, Hashable k) => k -> UnionFind k -> k
find key = setParent . findSet key

-- | Find the set that the given key is part of.
findSet :: (Eq k, Hashable k) => k -> UnionFind k -> Set k
findSet key (UnionFind hm) = go key
  where
    go cur = case HashMap.lookup cur hm of
      Nothing -> error "UnionFind.find: key was never added"
      Just set
        | setParent set == cur -> set
        | otherwise -> go (setParent set)

-- | The result of a union operation.
data Union k = Union
  { unionAbsorbed :: !k
    -- ^ The set representant that was absorbed as part of the union
  , unionInto :: !k
    -- ^ The set representant that the other set was merged into
  }

-- | Merge the two sets represented by the given keys
union :: (Eq k, Hashable k) => k -> k -> UnionFind k -> (Maybe (Union k), UnionFind k)
union key1 key2 uf@(UnionFind hm)
  -- The two keys already refer to the same set
  | setParent set1 == setParent set2 = (Nothing, uf)
  | setRank set1 < setRank set2 = merge set1 set2
  | otherwise = merge set2 set1
  where
    set1 = findSet key1 uf
    set2 = findSet key2 uf

    -- Insert the smaller set into the larger set
    merge (Set smallerId smallerRank) (Set largerId largerRank) =
      let
        uf' = UnionFind
          $ HashMap.insert smallerId (Set largerId smallerRank)
          $ if smallerRank == largerRank
              then HashMap.insert largerId (Set largerId (largerRank + 1)) hm
              else hm
      in
        (Just $ Union smallerId largerId, uf')

-- | Return the sets that are represented by this structure in an undefined order.
sets :: (Eq k, Hashable k) => UnionFind k -> [[k]]
sets uf@(UnionFind hm)
  = HashMap.elems
  $ HashMap.fromListWith (++)
    [ (rep, [el])
    | el <- HashMap.keys hm
    , let rep = find el uf
    ]
