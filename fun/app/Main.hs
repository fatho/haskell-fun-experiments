module Main where

import System.IO (hPrint, stderr)


import Fun.Ast

import qualified Fun.Graph as Graph

main :: IO ()
main = do
  let
    prog
      = let_ 0 (int 2)
      $ let_ 1 (plus (var 0) (plus (var 0) (int 10)))
      $ plus (var 1) (plus (var 0) (var 1))

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
int = ELit . VInt

plus :: Expr -> Expr -> Expr
plus a b = EOp $ OpPlus a b

let_ :: Int -> Expr -> Expr -> Expr
let_ v = ELet (Var v)

var :: Int -> Expr
var = EVar . Var
