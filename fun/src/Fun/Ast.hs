{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Fun.Ast where

import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

data Expr
  = ELit Value
  | EOp (Op Expr)
  | ELet Var Expr Expr
  | EVar Var
  deriving (Show)

data Op a
  = OpPlus !a !a
  | OpMinus !a !a
  | OpMult !a !a
  | OpDiv !a !a
  | OpLt !a !a
  | OpLeq !a !a
  | OpGt !a !a
  | OpGeq !a !a
  | OpEq !a !a
  | OpNeq !a !a
  | OpOr !a !a
  | OpAnd !a !a
  deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

data Value
  = VInt !Int
  | VBool !Bool
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

newtype Var = Var Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

run :: MonadFail m => Expr -> m Value
run = eval Map.empty

eval :: MonadFail m => Map Var Value -> Expr -> m Value
eval env expr = case expr of
  ELit v -> pure v
  EOp eop -> do
    vop <- traverse (eval env) eop
    evalOp vop
  ELet var ev body -> do
    v <- eval env ev
    eval (Map.insert var v env) body
  EVar var -> case Map.lookup var env of
    Nothing -> fail $ "Unbound variable " ++ show var
    Just val -> pure val

evalOp :: MonadFail m => Op Value -> m Value
evalOp = \case
  OpPlus a b -> op ((+) @Int) a b
  OpMinus a b -> op ((-) @Int) a b
  OpMult a b -> op ((*) @Int) a b
  OpDiv a b -> op (div @Int) a b
  OpLt a b -> op ((<) @Int) a b
  OpLeq a b -> op ((<=) @Int) a b
  OpGt a b -> op ((>) @Int) a b
  OpGeq a b -> op ((>=) @Int) a b
  OpEq a b -> op ((==) @Int) a b
  OpNeq a b -> op ((/=) @Int) a b
  OpOr a b -> op (||) a b
  OpAnd a b -> op (&&) a b

op :: (MonadFail m, Literal a, Literal b, Literal c) => (a -> b -> c) -> Value -> Value -> m Value
op f l r = fmap toLit $ f <$> fromLit l <*> fromLit r

class Literal a where
  fromLit :: MonadFail m => Value -> m a
  toLit :: a -> Value

instance Literal Int where
  fromLit = \case
    VInt i -> pure i
    other -> fail $ show other ++ " is not an int"
  toLit = VInt

instance Literal Bool where
  fromLit = \case
    VBool b -> pure b
    other -> fail $ show other ++ " is not a bool"
  toLit = VBool