{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Fun2.UtilSpec where

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import qualified Data.Vector.Primitive as VP

import Fun2.Util

spec :: Spec
spec = do
  describe "hashPrimVectorWithSalt" $ do
    it "is independent of slice" $ do
      let
        largeVec = VP.fromList @Int [0, 1, 2, 0, 1, 4]
        s1 = VP.slice 0 2 largeVec
        s2 = VP.slice 2 2 largeVec
        s3 = VP.slice 3 2 largeVec

      hashPrimVectorWithSalt 0 s1 `shouldBe` hashPrimVectorWithSalt 0 s3
      hashPrimVectorWithSalt 0 s1 `shouldNotBe` hashPrimVectorWithSalt 0 s2
