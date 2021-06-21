{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Fun.Graph where

import Control.Lens (foldlOf', toListOf)
import Control.Monad.Cont (ContT (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (..), evalState, runState)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Fun.Ast
import GHC.Generics (Generic)

data GExpr
  = GLit !Value
  | GFree !Var
  | GOp (Op Ref)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

refs :: Applicative f => (Ref -> f Ref) -> GExpr -> f GExpr
refs f = \case
  i@(GLit _) -> pure i
  v@(GFree _) -> pure v
  GOp rop -> GOp <$> traverse f rop

newtype Ref = Ref Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

succRef :: Ref -> Ref
succRef (Ref i) = Ref (i + 1)

data Graph = Graph
  { graphNext :: !Ref,
    graphIntern :: !(HashMap GExpr Ref),
    graphNodes :: !(HashMap Ref GExpr),
    graphParents :: !(HashMap Ref [Ref])
  }

freeVars :: Graph -> [Var]
freeVars g =
  [ v
    | GFree v <- HashMap.elems (graphNodes g)
  ]

graphInsert :: GExpr -> Graph -> (Ref, Graph)
graphInsert gexp g = case HashMap.lookup gexp (graphIntern g) of
  Nothing ->
    ( graphNext g,
      g
        { graphNext = succRef (graphNext g),
          graphIntern = HashMap.insert gexp (graphNext g) (graphIntern g),
          graphNodes = HashMap.insert (graphNext g) gexp (graphNodes g),
          graphParents =
            foldlOf'
              refs
              (\parents child -> HashMap.insertWith (++) child [graphNext g] parents)
              (graphParents g)
              gexp
        }
    )
  Just ref -> (ref, g)

graphEmpty :: Graph
graphEmpty =
  Graph
    { graphNext = Ref 0,
      graphNodes = HashMap.empty,
      graphIntern = HashMap.empty,
      graphParents = HashMap.empty
    }

fromAst :: Expr -> (Ref, Graph)
fromAst root = runState (runReaderT (go root) HashMap.empty) graphEmpty
  where
    go expr = case expr of
      ELit i -> prim $ GLit i
      EOp eop -> do
        gop <- traverse go eop
        prim $ GOp gop
      EVar v ->
        asks (HashMap.lookup v) >>= \case
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
          GLit i -> pure $ ELit i
          GFree v -> pure $ EVar v
          GOp gop -> do
            eop <- traverse go gop
            pure $ EOp eop

toAstWithSharing :: (Ref, Graph) -> Expr
toAstWithSharing (root, g) = evalState (runReaderT (go root pure) HashMap.empty) (Var 0)
  where
    go ref cont =
      asks (HashMap.lookup ref) >>= \case
        -- Not shared
        Nothing -> do
          case HashMap.lookup ref (graphNodes g) of
            Nothing -> error $ "Invariant violated: unknown graph ref " ++ show ref
            Just gexpr -> case gexpr of
              GLit i -> shared ref (ELit i) cont
              GFree v -> shared ref (EVar v) cont
              GOp gop ->
                runContT (traverse (ContT . go) gop) $ \eop -> shared ref (EOp eop) cont
        -- Shared
        Just expr -> cont expr

    free = HashSet.fromList $ freeVars g

    shared ref expr cont = do
      let numUses = maybe 0 length $ HashMap.lookup ref (graphParents g)
      if numUses > 1 && not (shouldInline expr)
        then do
          var <- freshVar
          local (HashMap.insert ref (EVar var)) $ do
            inner <- cont (EVar var)
            pure $ ELet var expr inner
        else do
          cont expr

    shouldInline = \case
      ELit {} -> True
      EVar {} -> True
      _ -> False

    freshVar = do
      next@(Var nextId) <- get
      put $! Var $! nextId + 1
      if HashSet.member next free
        then freshVar
        else pure next

dot :: Graph -> String
dot g = unlines $ header ++ nodes ++ footer
  where
    header = ["digraph ast {"]
    nodes = concatMap (map (indent 2) . mkNode) (HashMap.toList $ graphNodes g)
    footer = ["}"]

    indent i = (replicate i ' ' ++)

    mkNode (ref, gexpr) =
      let label = case gexpr of
            GLit i -> show i
            GOp gop -> case gop of
              OpPlus {} -> "+"
              OpMinus {} -> "-"
              OpMult {} -> "*"
              OpDiv {} -> "/"
              OpLt {} -> "<"
              OpLeq {} -> "<="
              OpGt {} -> ">"
              OpGeq {} -> ">="
              OpEq {} -> "=="
              OpNeq {} -> "!="
              OpOr {} -> "||"
              OpAnd {} -> "&&"
            GFree v -> show v
          targets = toListOf refs gexpr
       in (mkNodeId ref ++ "[label=" ++ show label ++ "];") :
            [ mkNodeId ref ++ " -> " ++ mkNodeId target
              | target <- targets
            ]

    mkNodeId (Ref ref) = "node_" ++ show ref
