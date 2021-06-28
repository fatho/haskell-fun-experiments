module Fun2.Dot where

import Control.Lens (view)
import Data.Hashable (Hashable)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Primitive as VP

import Fun2.Graph

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
      , let repr = find ref g
      ]

    allNodes = HashMap.toList $ view nodes g

    indent i = (replicate i ' ' ++)

    mkCluster (Ref reprId, clusterNodes) =
      let
        clusterHeader = [ "subgraph cluster_" ++ show reprId ++ " {" ]
        nodeLines = concatMap (map (indent 2) . mkNode) clusterNodes
        clusterFooter = ["}"]
      in
        clusterHeader ++ nodeLines ++ clusterFooter

    mkNode (ref, nodeData) =
      let Node label _ = view nodeSelf nodeData
       in [mkNodeId ref ++ "[label=" ++ show label ++ "];"]

    mkEdges (ref, nodeData) =
      let Node _ children = view nodeSelf nodeData
       in [ edgeLine
          | (index, target) <- zip [0 :: Int ..] $ VP.toList children
          , edgeLine <- mkEdge index ref target
          ]

    mkEdge index from to
      -- If the node points to the same cluster, we need an invisible extra node
      | toRepr == find from g =
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
        toRepr@(Ref toReprId) = find to g


    mkNodeId (Ref ref) = "node_" ++ show ref
