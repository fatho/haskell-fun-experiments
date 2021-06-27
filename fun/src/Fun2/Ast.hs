{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Fun2.Ast where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data Node
  = Lit Lit
  | BoundVar Bound
  | FreeVar Free
  | Plus
  | Mult
  | Eq
  | Lam
  | App
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

data Lit
  = LInt !Int
  | LBool !Bool
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

newtype Bound = Bound Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

newtype Free = Free String
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)
