{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad.State.Strict (runState, state, modify, gets)
import System.IO (hPrint, stderr)

import Fun2.Graph (Node (..), Ref (..))
import qualified Fun2.Graph as Graph
import qualified Fun2.Dot as Dot

main :: IO ()
main = do
  let
    (info, g) = flip runState Graph.empty $ do
      ref1 <- state $ Graph.insert (Node "0" [])
      ref2 <- state $ Graph.insert (Node "1" [])
      ref3 <- state $ Graph.insert (Node "+" [ref1, ref2])
      ref4 <- state $ Graph.insert (Node "+" [ref1, ref3])
      ref5 <- state $ Graph.insert (Node "+" [ref1, ref4])
      ref6 <- state $ Graph.insert (Node "+" [ref1, ref5])
      ref7 <- state $ Graph.insert (Node "+" [ref1, ref6])
      -- -- 1 + 0 == 0, so we can union the two
      modify $ Graph.union ref2 ref3

      let
        refs = [ref1, ref2, ref3, ref4, ref5, ref6, ref7] :: [Ref]
      (,) refs <$> traverse (gets . Graph.find) refs

  hPrint stderr info
  putStrLn $ Dot.dot g


-- import Fun.Ast

-- import qualified Fun.Graph as Graph

-- main :: IO ()
-- main = do
--   let
--     prog1
--       = let_ 0 (int 2)
--       $ let_ 1 (plus (var 0) (plus (var 0) (int 10)))
--       $ plus (var 1) (plus (var 0) (var 1))


--     prog2
--       = let_ 0 (int 2)
--       $ let_ 1 (lam 3 $ plus (var 3) (plus (var 3) (int 10)))
--       $ let_ 2 (app (var 1) (var 0))
--       $ let_ 3 (lam 4 $ plus (var 4) (int 10))
--       $ plus (var 2) (plus (var 0) (var 2))


--     prog3
--       = lam 0
--       $ lam 1
--       $ plus
--           (plus (plus (var 0) (var 0)) (var 1))
--           (plus (plus (var 0) (var 0)) (plus (int 1) (var 1)))

--     prog = prog1

--     (root, graph) = Graph.fromAst prog

--     prog' = Graph.toAst (root, graph)
--     prog'' = Graph.toAstWithSharing (root, graph)

--   putStrLn $ Graph.dotClusters $ Graph.saturate graph
--   hPrint stderr prog'
--   hPrint stderr prog''
--   run prog >>= hPrint stderr
--   run prog' >>= hPrint stderr
--   run prog'' >>= hPrint stderr

-- int :: Int -> Expr
-- int = ELit . LInt

-- plus :: Expr -> Expr -> Expr
-- plus a b = EOp $ OpPlus a b

-- let_ :: Int -> Expr -> Expr -> Expr
-- let_ v = ELet (Var v)

-- lam :: Int -> Expr -> Expr
-- lam v = ELam (Var v)

-- app :: Expr -> Expr -> Expr
-- app = EApp

-- var :: Int -> Expr
-- var = EVar . Var
