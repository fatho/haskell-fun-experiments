{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Fun.Graph where

import Control.Lens (toListOf)
import Control.Monad.State (runState, MonadState (..), evalState)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (..), asks)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap

import Fun.Ast
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet

data GExpr
  = GInt !Int
  | GFree !Var
  | GPlus !Ref !Ref
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

refs :: Applicative f => (Ref -> f Ref) -> GExpr -> f GExpr
refs f = \case
  i@(GInt _) -> pure i
  v@(GFree _) -> pure v
  GPlus r1 r2 -> GPlus <$> f r1 <*> f r2

newtype Ref = Ref Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

succRef :: Ref -> Ref
succRef (Ref i) = Ref (i + 1)

data Graph = Graph
  { graphNext :: !Ref
  , graphIntern :: !(HashMap GExpr Ref)
  , graphNodes :: !(HashMap Ref GExpr)
  }

freeVars :: Graph -> [Var]
freeVars g =
  [ v
  | GFree v <- HashMap.elems (graphNodes g)
  ]

graphInsert :: GExpr -> Graph -> (Ref, Graph)
graphInsert gexp g = case HashMap.lookup gexp (graphIntern g) of
  Nothing ->
    ( graphNext g
    , g
      { graphNext = succRef (graphNext g)
      , graphIntern = HashMap.insert gexp (graphNext g) (graphIntern g)
      , graphNodes = HashMap.insert (graphNext g) gexp (graphNodes g)
      }
    )
  Just ref -> (ref, g)

graphEmpty :: Graph
graphEmpty = Graph
  { graphNext = Ref 0
  , graphNodes = HashMap.empty
  , graphIntern = HashMap.empty
  }

fromAst :: Expr -> (Ref, Graph)
fromAst root = runState (runReaderT (go root) HashMap.empty) graphEmpty
  where
    go expr = case expr of
      EInt i -> prim $ GInt i
      EPlus e1 e2 -> do
        g1 <- go e1
        g2 <- go e2
        prim $ GPlus g1 g2
      EVar v -> asks (HashMap.lookup v) >>= \case
        -- Free variable
        Nothing -> prim $ GFree v
        -- Bound variable
        Just ref -> pure ref
      ELet v e1 e2 -> do
        g1 <- go e1
        local (HashMap.insert v g1) $ go e2

    prim gexpr = state (graphInsert gexpr)

toAst :: (Ref, Graph) -> Expr
toAst (root, g) = evalState (go root) (Var 0)
  where
    go ref = do
      case HashMap.lookup ref (graphNodes g) of
        Nothing -> error $ "Invariant violated: unknown graph ref " ++ show ref
        Just gexpr -> case gexpr of
          GInt i -> pure $ EInt i
          GFree v -> pure $ EVar v
          GPlus g1 g2 -> do
            e1 <- go g1
            e2 <- go g2
            pure $ EPlus e1 e2

    -- free = HashSet.fromList $ freeVars g

    -- freshVar = do
    --   next <- get
    --   put $! next + 1
    --   if HashSet.member next free
    --     then freshVar
    --     else pure next

dot :: Graph -> String
dot g = unlines $ header ++ nodes ++ footer
  where
    header = ["digraph ast {"]
    nodes = concatMap (map (indent 2) . mkNode) (HashMap.toList $ graphNodes g)
    footer = ["}"]

    indent i = (replicate i ' ' ++)

    mkNode (ref, gexpr) =
      let
        label = case gexpr of
          GInt i -> show i
          GPlus{} -> "+"
          GFree v -> show v
        targets = toListOf refs gexpr
      in
        (mkNodeId ref ++ "[label=" ++ show label ++ "];") :
        [ mkNodeId ref ++ " -> " ++ mkNodeId target
        | target <- targets
        ]

    mkNodeId (Ref ref) = "node_" ++ show ref
