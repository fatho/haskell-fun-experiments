module Main where

import System.IO (hPrint, stderr)


import Fun.Ast

import qualified Fun.Graph as Graph

main :: IO ()
main = do
  let
    prog1
      = let_ 0 (int 2)
      $ let_ 1 (plus (var 0) (plus (var 0) (int 10)))
      $ plus (var 1) (plus (var 0) (var 1))


    prog2
      = let_ 0 (int 2)
      $ let_ 1 (lam 3 $ plus (var 3) (plus (var 3) (int 10)))
      $ let_ 2 (app (var 1) (var 0))
      $ let_ 3 (lam 4 $ plus (var 4) (int 10))
      $ plus (var 2) (plus (var 0) (var 2))


    prog3
      = lam 0
      $ lam 1
      $ plus
          (plus (plus (var 0) (var 0)) (var 1))
          (plus (plus (var 0) (var 0)) (plus (int 1) (var 1)))

    prog = prog2

    (root, graph) = Graph.fromAst prog

    prog' = Graph.toAst (root, graph)
    prog'' = Graph.toAstWithSharing (root, graph)

  putStrLn $ Graph.dot graph
  hPrint stderr prog'
  hPrint stderr prog''
  run prog >>= hPrint stderr
  run prog' >>= hPrint stderr
  run prog'' >>= hPrint stderr

int :: Int -> Expr
int = ELit . LInt

plus :: Expr -> Expr -> Expr
plus a b = EOp $ OpPlus a b

let_ :: Int -> Expr -> Expr -> Expr
let_ v = ELet (Var v)

lam :: Int -> Expr -> Expr
lam v = ELam (Var v)

app :: Expr -> Expr -> Expr
app = EApp

var :: Int -> Expr
var = EVar . Var
