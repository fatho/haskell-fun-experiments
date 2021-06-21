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
  OpPlus a b -> op AsInt AsInt VInt (+) a b
  OpMinus a b -> op AsInt AsInt VInt (-) a b
  OpMult a b -> op AsInt AsInt VInt (*) a b
  OpDiv a b -> op AsInt AsInt VInt div a b
  OpLt a b -> op AsInt AsInt VBool (<) a b
  OpLeq a b -> op AsInt AsInt VBool (<=) a b
  OpGt a b -> op AsInt AsInt VBool (>) a b
  OpGeq a b -> op AsInt AsInt VBool (>=) a b
  OpEq a b -> op AsInt AsInt VBool (==) a b
  OpNeq a b -> op AsInt AsInt VBool (/=) a b
  OpOr a b -> op AsBool AsBool VBool (||) a b
  OpAnd a b -> op AsBool AsBool VBool (&&) a b

op :: MonadFail m => As a -> As b -> (c -> Value) -> (a -> b -> c) -> Value -> Value -> m Value
op lt rt out f l r = fmap out $ f <$> as lt l <*> as rt r

as :: MonadFail m => As a -> Value -> m a
as AsInt = \case
  VInt i -> pure i
  other -> fail $ show other ++ " is not an int"
as AsBool = \case
  VBool b -> pure b
  other -> fail $ show other ++ " is not a bool"

data As a where
  AsInt :: As Int
  AsBool :: As Bool