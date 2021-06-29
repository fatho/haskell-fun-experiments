{-# LANGUAGE TypeApplications #-}

module Fun2.UnionFindSpec where

import Data.Hashable (Hashable)
import Data.List (sort)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), forAll)
import Test.QuickCheck.Gen (Gen)

import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck.Modifiers as Mod

import qualified Fun2.UnionFind as UF

spec :: Spec
spec = do
  describe "UnionFind" $ do
    it "empties" $ do
      UF.sets (UF.empty @Int) `shouldBe` []
    it "inserts" $ do
      let
        uf0 = UF.empty @Int
        uf1 = UF.insert 1 uf0
        uf2 = UF.insert 2 uf1

      sort (UF.sets uf2) `shouldBe` [[1], [2]]

    it "finds" $ do
      let
        uf0 = UF.empty @Int
        uf1 = UF.insert 1 uf0
        uf2 = UF.insert 2 uf1

      UF.find 1 uf2 `shouldBe` 1
      UF.find 2 uf2 `shouldBe` 2

    it "unions" $ do
      let
        uf0 = foldr UF.insert (UF.empty @Int) [0..9]
        uf1 = foldl (flip $ uncurry UF.union) uf0 [(1, 3), (3, 5), (7, 9), (5, 7)]
        uf2 = foldl (flip $ uncurry UF.union) uf1 [(0, 8), (2, 6), (4, 2), (8, 6)]

      sort² (UF.sets uf2) `shouldBe` [[0,2,4,6,8],[1,3,5,7,9]]

    context "properties" $ do
      prop "find X (insert X empty) == X" $ \item ->
        UF.find (item :: Int) (UF.insert item UF.empty) == item

      prop "find X (union X Y ...) == find Y (union X Y ...)" $ \x y ->
        let uf = UF.union x y $ UF.insert x $ UF.insert y UF.empty
        in UF.find (x :: Int) uf == UF.find y uf

      prop "union commutative" $
        \(Mod.NonEmpty items) -> forAll (pairsOf @Int items) $ \unions -> forAll (pairOf items) $ \(x, y) ->
          let uf = unionFindOf items unions
          in UF.union x y uf == UF.union y x uf

      prop "union idempotent" $
        \(Mod.NonEmpty items) -> forAll (pairsOf @Int items) $ \unions -> forAll (pairOf items) $ \(x, y) ->
          let uf = UF.union x y $ unionFindOf items unions
          in UF.union x y uf == uf

      prop "insert idempotent" $
        \(Mod.NonEmpty items) -> forAll (pairsOf @Int items) $ \unions -> forAll (Gen.elements items) $ \item ->
          let uf = unionFindOf items unions
          in UF.insert item uf == uf


pairsOf :: [k] -> Gen [(k, k)]
pairsOf items = Gen.sublistOf $ (,) <$> items <*> items

unionFindOf :: (Eq k, Hashable k) => [k] -> [(k, k)] -> UF.UnionFind k
unionFindOf items unions =
  let uf0 = foldr UF.insert UF.empty items
  in foldr (uncurry UF.union) uf0 unions

pairOf :: [k] -> Gen (k, k)
pairOf items = (,) <$> Gen.elements items <*> Gen.elements items

genUnionFind :: (Arbitrary k, Eq k, Hashable k) => Gen (UF.UnionFind k)
genUnionFind = do
  items <- Gen.listOf arbitrary
  let uf0 = foldr UF.insert UF.empty items
  unions <- Gen.sublistOf $ (,) <$> items <*> items
  pure $ foldr (uncurry UF.union) uf0 unions

sort² :: Ord k => [[k]] -> [[k]]
sort² = sort . map sort
