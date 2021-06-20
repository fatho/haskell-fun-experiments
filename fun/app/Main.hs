module Main where

import System.IO (hPrint, stderr)


import Fun.Ast

import qualified Fun.Graph as Graph

main :: IO ()
main = do
  let
    prog
      = ELet (Var 0) (EInt 2)
      $ ELet (Var 1) (EPlus (EVar (Var 0)) (EPlus (EVar (Var 0)) (EInt 10)))
      $ EPlus (EVar (Var 1)) (EPlus (EVar (Var 0)) (EVar (Var 1)))

    (root, graph) = Graph.fromAst prog

  putStrLn $ Graph.dot graph
  hPrint stderr (Graph.toAst (root, graph))
  hPrint stderr (Graph.toAstWithSharing (root, graph))
