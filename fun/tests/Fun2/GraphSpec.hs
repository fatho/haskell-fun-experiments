{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Fun2.GraphSpec where

import Control.Monad.State.Strict (State, runState, state, modify)
import Data.Hashable (Hashable (hash))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import qualified Data.Vector.Primitive as VP

import Fun2.Graph (Graph, Node (..), Ref (..))
import qualified Fun2.Graph as Graph

spec :: Spec
spec = do
  describe "Nodes" $ do
    it "can be hashed" $ do
      let
        largeVec = VP.fromList [Ref 0, Ref 1, Ref 2, Ref 0, Ref 1, Ref 4]
        -- make sure the hashing works independent of where in the underlying
        -- bytearray the vector points
        n1 = Node "+" (VP.slice 0 2 largeVec)
        n2 = Node "+" (VP.slice 2 2 largeVec)
        n3 = Node "+" (VP.slice 3 2 largeVec)

      hash n1 `shouldBe` hash n3
      hash n1 `shouldNotBe` hash n2

  describe "Graph" $ do
    it "interns" $ do
      let
        g0 = Graph.empty
        (ref1, g1) = Graph.insert (Node "0" []) g0
        (ref2, g2) = Graph.insert (Node "1" []) g1
        (ref3, g3) = Graph.insert (Node "+" [ref1, ref2]) g2
        (ref4, g4) = Graph.insert (Node "1" []) g3
        (ref5, g5) = Graph.insert (Node "+" [ref1, ref4]) g4
        (ref6, g6) = Graph.insert (Node "+" [ref4, ref1]) g5

      ref2 `shouldBe` ref4
      ref3 `shouldBe` ref5
      g3 `shouldBe` g5
      ref5 `shouldNotBe` ref6
      g5 `shouldNotBe` g6

    it "finds singleton sets" $ do
      let
        ((ref1, ref2, ref3), g) = buildGraph $ do
          ref1 <- state $ Graph.insert (Node "0" [])
          ref2 <- state $ Graph.insert (Node "1" [])
          ref3 <- state $ Graph.insert (Node "+" [ref1, ref2])
          pure (ref1, ref2, ref3)

      Graph.find ref1 g `shouldBe` ref1
      Graph.find ref2 g `shouldBe` ref2
      Graph.find ref3 g `shouldBe` ref3

    it "unions" $ do
      let
        ((ref1, ref2, ref3), g) = buildGraph $ do
          ref1 <- state $ Graph.insert (Node "0" [])
          ref2 <- state $ Graph.insert (Node "1" [])
          ref3 <- state $ Graph.insert (Node "+" [ref1, ref2])
          ref4 <- state $ Graph.insert (Node "+" [ref1, ref3])
          -- 1 + 0 == 0, so we can union the two
          modify $ Graph.union ref2 ref3
          pure (ref1, ref2, ref3)

      Graph.find ref1 g `shouldBe` ref1
      Graph.find ref2 g `shouldBe` Graph.find ref3 g
      Graph.find ref3 g `shouldBe` Graph.find ref2 g


buildGraph :: State (Graph n) a -> (a, Graph n)
buildGraph = flip runState Graph.empty
