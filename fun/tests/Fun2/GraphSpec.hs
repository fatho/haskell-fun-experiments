{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Fun2.GraphSpec where

import Control.Monad (unless)
import Control.Monad.State.Strict (State, runState, state, modify)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), shrinkList)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector.Primitive as VP
import qualified Test.QuickCheck.Gen as Gen

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

    it "equalizes new nodes" $ do
      let
        ((ref0, ref1, ref2, ref3), g) = buildGraph $ do
          ref0 <- state $ Graph.insert (Node "0" [])
          ref1 <- state $ Graph.insert (Node "1" [])
          ref2 <- state $ Graph.insert (Node "+" [ref0, ref1])
          -- 1 + 0 == 0, so we can equalize the two
          modify $ Graph.equalize ref1 ref2

          -- This new node should be equivalent to ref2,
          -- since it also adds 0 and the [ref1 ref2] class.
          ref3 <- state $ Graph.insert (Node "+" [ref0, ref2])

          pure (ref0, ref1, ref2, ref3)

      Graph.classOf ref0 g `shouldBe` ref0
      Graph.classes g `shouldBe`
        [ [ (ref0,Node "0" []) ]
        , [ (ref1,Node "1" [])
          , (ref2,Node "+" [ref0, ref1])
          , (ref3,Node "+" [ref0, ref2]) ]
        ]

    prop "keeps up invariants" $ \(GraphOps ops) -> checkGraph ops


data GraphOp
  = OpInsert Int String [Int]
  | OpEqualize Int Int
  deriving (Eq, Show)

newtype GraphOps = GraphOps [GraphOp]
  deriving (Show)

instance Arbitrary GraphOps where
  arbitrary = go IntSet.empty []
    where
      vars = ['a'..'z']

      go refs ops = Gen.frequency
        [ (1, pure $ GraphOps $ reverse ops )
        , (10, do
            if IntSet.null refs
              then go refs ops
              else do
                a <- Gen.elements $ IntSet.toList refs
                b <- Gen.elements $ IntSet.toList refs
                go refs (OpEqualize a b : ops)
          )
        , (25, do
            let key = IntSet.size refs
            var <- Gen.elements vars
            let op = OpInsert key [var] []
            go (IntSet.insert key refs) (op : ops)
          )
        , (25, do
            if IntSet.null refs
              then go refs ops
              else do
                let key = IntSet.size refs
                a <- Gen.elements $ IntSet.toList refs
                b <- Gen.elements $ IntSet.toList refs
                let op = OpInsert key "+" [a, b]
                go (IntSet.insert key refs) (op : ops)
          )
        ]

  shrink (GraphOps ops) = map (GraphOps . fixup) $ shrinkList pure ops
    where
      fixup = go IntSet.empty
      go _ [] = []
      go validRefs (op:ops) =
        case op of
          OpEqualize a b
            | IntSet.member a validRefs && IntSet.member b validRefs
              -> op : go validRefs ops
            | otherwise -> go validRefs ops
          OpInsert key _ _ -> op : go (IntSet.insert key validRefs) ops

            
  

buildGraph :: State (Graph n) a -> (a, Graph n)
buildGraph = flip runState Graph.empty


checkGraph :: [GraphOp] -> IO ()
checkGraph = go Graph.empty IntMap.empty []
  where
    go g _ _ [] = do
      writeFile "/tmp/g.dot" $ Graph.dot g
    go g refs applied (op:ops) = case op of
      OpInsert key nodeType children -> do
        let
          children' = VP.fromList $ map (refs IntMap.!) children
          (ref, g') = Graph.insert (Node nodeType children') g
        goCheck g' (IntMap.insert key ref refs) applied op ops
      OpEqualize a b -> do
        let
          ra = refs IntMap.! a
          rb = refs IntMap.! b
          g' = Graph.equalize ra rb g
        goCheck g' refs applied op ops
      
    goCheck g refs applied op ops = do
      let invariantFailures = Graph.checkInvariants g

      unless (null invariantFailures) $ do
        fail $ "Graph invariant violated by applying " ++
          show op ++ " after " ++ show (reverse applied)

      go g refs (op : applied) ops
