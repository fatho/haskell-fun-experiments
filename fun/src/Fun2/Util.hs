{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
module Fun2.Util
  ( hashPrimVectorWithSalt )
  where

import qualified Data.Hashable as Hashable
import qualified Data.Primitive as P
import qualified Data.Vector.Primitive as VP

hashPrimVectorWithSalt :: forall a. VP.Prim a => Int -> VP.Vector a -> Int
hashPrimVectorWithSalt salt (VP.Vector offset len (P.ByteArray arr#)) =
  let
    elemSize = P.sizeOf (undefined :: a)
    byteOff = offset * elemSize
    byteLen = len * elemSize
  in
    Hashable.hashByteArrayWithSalt arr# byteOff byteLen salt
