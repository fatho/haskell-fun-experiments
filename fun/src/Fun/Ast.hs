{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Fun.Ast where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Expr
  = EInt Int
  | EPlus Expr Expr
  | ELet Var Expr Expr
  | EVar Var

newtype Var = Var Int
  deriving (Eq, Ord, Show)


run :: MonadFail m => Expr -> m Int
run = eval Map.empty

eval :: MonadFail m => Map Var Int -> Expr -> m Int
eval env expr = case expr of
  EInt i -> pure i
  EPlus e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    pure $! v1 + v2
  ELet var ev body -> do
    v <- eval env ev
    eval (Map.insert var v env) body
  EVar var -> case Map.lookup var env of
    Nothing -> fail $ "Unbound variable " ++ show var
    Just val -> pure val
