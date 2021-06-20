module Main where

import Lib (fun)

import Fun.Ast

main :: IO ()
main = do
  let
    prog
      = ELet (Var 0) (EInt 2)
      $ ELet (Var 1) (EPlus (EVar (Var 0)) (EPlus (EVar (Var 0)) (EInt 10)))
      $ EPlus (EVar (Var 0)) (EVar (Var 1))

  run prog >>= print
  fun
