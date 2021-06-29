{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Fun2.GraphSpec where

import Control.Monad.State.Strict (State, runState, state, modify)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import Fun2.Graph (Graph, Node (..))
import qualified Fun2.Graph as Graph

spec :: Spec
spec = do
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

      Graph.classOf ref1 g `shouldBe` ref1
      Graph.classOf ref2 g `shouldBe` ref2
      Graph.classOf ref3 g `shouldBe` ref3

    it "equalizes" $ do
      let
        ((ref0, ref1, ref2, ref3), g) = buildGraph $ do
          ref0 <- state $ Graph.insert (Node "0" [])
          ref1 <- state $ Graph.insert (Node "1" [])
          ref2 <- state $ Graph.insert (Node "+" [ref0, ref1])
          ref3 <- state $ Graph.insert (Node "+" [ref0, ref2])
          -- 1 + 0 == 0, so we can equalize the two
          modify $ Graph.equalize ref1 ref2
          pure (ref0, ref1, ref2, ref3)

      Graph.classOf ref0 g `shouldBe` ref0
      Graph.classes g `shouldBe`
        [ [ (ref0,Node "0" []) ]
        , [ (ref1,Node "1" [])
          , (ref2,Node "+" [ref0, ref1])
          , (ref3,Node "+" [ref0, ref2]) ]
        ]


buildGraph :: State (Graph n) a -> (a, Graph n)
buildGraph = flip runState Graph.empty
