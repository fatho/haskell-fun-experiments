{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Fun.Graph where

import Control.Lens (At (at), _1, _3, foldlOf', makeLenses, over, toListOf, traversed, use, (%=),
                     (.=), (<<+=))
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), asks)
import Control.Monad.State.Strict (MonadState (..), State, evalState, runState)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List

import Data.Foldable (for_)
import Fun.Ast

data GExpr
  = GLit !Lit
  | GFree !Var
  | GOp (Op Ref)
  | GArg !Int -- ^ De Bruijn index of the argument variable
  | GLam !Ref
  | GApp !Ref !Ref
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

refs :: Applicative f => (Ref -> f Ref) -> GExpr -> f GExpr
refs f = \case
  i@(GLit _)  -> pure i
  v@(GFree _) -> pure v
  GOp rop     -> GOp <$> traverse f rop
  a@(GArg _)  -> pure a
  GLam r      -> GLam <$> f r
  GApp r1 r2  -> GApp <$> f r1 <*> f r2

newtype Ref = Ref Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

succRef :: Ref -> Ref
succRef (Ref i) = Ref (i + 1)

data Graph = Graph
  { graphNext    :: !Ref,
    graphIntern  :: !(HashMap GExpr Ref),
    graphNodes   :: !(HashMap Ref GExpr),
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

data AstVar = AstVarLet Ref | AstVarArg

fromAst :: Expr -> (Ref, Graph)
fromAst root = runState (runReaderT (go [] root) HashMap.empty) graphEmpty
  where
    go args expr = case expr of
      ELit i -> prim $ GLit i
      EOp eop -> do
        gop <- traverse (go args) eop
        prim $ GOp gop
      EVar v
        | Just index <- List.elemIndex v args ->
            -- Lambda-bound variable
            prim $ GArg index
        | otherwise -> asks (HashMap.lookup v) >>= \case
            -- Free variable
            Nothing -> prim $ GFree v
            -- Let-Bound variable
            Just ref -> pure ref
      ELet v e1 e2 -> do
        g1 <- go args e1
        local (HashMap.insert v g1) $ go args e2
      ELam v ebody -> do
        body <- go (v : args) ebody
        prim $ GLam body
      EApp ef ea -> do
        gf <- go args ef
        ga <- go args ea
        prim $ GApp gf ga

    prim gexpr = state (graphInsert gexpr)

toAst :: (Ref, Graph) -> Expr
toAst (root, g) = evalState (go [] root) (Var 0)
  where
    go args ref = do
      case HashMap.lookup ref (graphNodes g) of
        Nothing -> error $ "Invariant violated: unknown graph ref " ++ show ref
        Just gexpr -> case gexpr of
          GLit i -> pure $ ELit i
          GFree v -> pure $ EVar v
          GOp gop -> do
            eop <- traverse (go args) gop
            pure $ EOp eop
          GApp gf ga -> do
            ef <- go args gf
            ea <- go args ga
            pure $ EApp ef ea
          GArg index ->
            pure $ EVar $ args !! index
          GLam gbody -> do
            v <- freshVar
            ebody <- go (v : args) gbody
            pure $ ELam v ebody

    free = HashSet.fromList $ freeVars g

    freshVar = do
      next@(Var nextId) <- get
      put $! Var $! nextId + 1
      if HashSet.member next free
        then freshVar
        else pure next

data ToAstState = ToAstState
  { _nextVarId :: Int
  , _sharing   :: HashMap Ref Var
  , _bindings  :: [(Ref, Var, Int, Expr)]
  }

makeLenses ''ToAstState

type ToAst = State ToAstState

toAstWithSharing :: (Ref, Graph) -> Expr
toAstWithSharing (root, g) = snd $ evalState (limitScope $ go [] root) $ ToAstState
  { _nextVarId = 0
  , _sharing = mempty
  , _bindings = mempty
  }
  where
    limitScope :: ToAst (HashSet Int, Expr) -> ToAst (HashSet Int, Expr)
    limitScope act = do
      outerBindings <- use bindings
      bindings .= mempty

      (indices, expr) <- act

      nestedBindings <- use bindings
      bindings .= outerBindings

      let
        bind inner (_, boundVar, _, boundExpr) = ELet boundVar boundExpr inner
        exprWithBindings = List.foldl' bind expr nestedBindings

      pure (indices, exprWithBindings)

    go :: [Var] -> Ref -> ToAst (HashSet Int, Expr)
    go args ref = shared ref $ case HashMap.lookup ref (graphNodes g) of
      Nothing -> error $ "Invariant violated: unknown graph ref " ++ show ref
      Just gexpr -> case gexpr of
        GLit i -> pure (mempty, ELit i)
        GFree v -> pure (mempty, EVar v)
        GOp gop -> do
          eop <- traverse (go args) gop
          let indices = HashSet.unions $ toListOf (traversed . _1) eop
          pure (indices, EOp $! fmap snd eop)
        GApp gf ga -> do
          (bf, ef) <- go args gf
          (ba, ea) <- go args ga
          pure (min bf ba, EApp ef ea)
        GArg index ->
          pure (HashSet.singleton index, EVar $ args !! index)
        GLam gbody -> do
          v <- freshVar

          outerBindings <- use bindings
          bindings .= mempty

          (bindices, ebody) <- go (v : args) gbody

          nestedBindings <- use bindings

          let
            (lambdaBindings, liftableBindings) = List.partition (\(_, _, lvl, _) -> lvl == 0) nestedBindings
            liftedBindings = map (over _3 pred) liftableBindings

            bind inner (_, boundVar, _, expr) = ELet boundVar expr inner
            bodyWithBindings = List.foldl' bind ebody lambdaBindings

          for_ lambdaBindings $ \(boundRef, _, _,  _) ->
            sharing %= HashMap.delete boundRef
          bindings .= liftedBindings ++ outerBindings

          let indices = HashSet.map (subtract 1) $ HashSet.delete 0 bindices
          pure (indices, ELam v bodyWithBindings)

    free = HashSet.fromList $ freeVars g

    shared ref generate = use (sharing . at ref) >>= \case
      Nothing -> do
        -- Not yet visited
        let numUses = maybe 0 length $ HashMap.lookup ref (graphParents g)
        (indices, expr) <- generate
        if numUses > 1 && not (shouldInline expr)
          then do
            var <- freshVar
            bindings %= (:) (ref, var, minimum $ maxBound : HashSet.toList indices, expr)
            sharing . at ref .= Just var
            pure (mempty, EVar var)
          else
            pure (indices, expr)
      Just var -> pure (mempty, EVar var)


    shouldInline = \case
      ELit {} -> True
      EVar {} -> True
      _       -> False

    freshVar = do
      next <- fmap Var $ nextVarId <<+= 1
      if HashSet.member next free
        then freshVar
        else pure next

dot :: Graph -> String
dot g = unlines $ header ++ nodes ++ footer
  where
    header =
        [ "digraph ast {"
        , "  compound=true;"
        ]
    nodes = concatMap (map (indent 2) . mkNode) (HashMap.toList $ graphNodes g)
    footer = ["}"]

    indent i = (replicate i ' ' ++)

    mkNode (ref, gexpr) =
      let label = case gexpr of
            GLit i -> show i
            GOp gop -> case gop of
              OpPlus {}  -> "+"
              OpMinus {} -> "-"
              OpMult {}  -> "*"
              OpDiv {}   -> "/"
              OpLt {}    -> "<"
              OpLeq {}   -> "<="
              OpGt {}    -> ">"
              OpGeq {}   -> ">="
              OpEq {}    -> "=="
              OpNeq {}   -> "!="
              OpOr {}    -> "||"
              OpAnd {}   -> "&&"
            GFree v    -> show v
            GArg index -> "arg " ++ show index
            GLam{}     -> "&#955;"
            GApp{}     -> "@"
          targets = toListOf refs gexpr
       in (mkNodeId ref ++ "[label=" ++ show label ++ "];") :
            [ mkNodeId ref ++ " -> " ++ mkNodeId target
              | target <- targets
            ]

    mkNodeId (Ref ref) = "node_" ++ show ref
