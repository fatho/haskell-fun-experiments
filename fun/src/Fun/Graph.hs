{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Fun.Graph where

import Data.HashMap.Strict (HashMap)

import Fun.Ast

data GExpr
  = GInt Int
  | GFree Var
  | GPlus Ref Ref

newtype Ref = Ref Int

data Graph = Graph
  { graphNext :: !Ref
  , graphNodes :: HashMap Expr Ref
  }
