{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Set
import Relude


type Name = Text


data Expr where
  Var :: Text -> Expr
  Abs :: Text -> Expr -> Expr
  App :: Expr -> Expr -> Expr
  deriving (Eq, Show)


main :: IO ()
main = print $ step $ App (Abs "x" (Abs "y" (Var "x"))) (Var "y")


fresh :: Name -> Expr -> Name
fresh x s
  | x `member` fv s = fresh (x <> "'") s
  | otherwise = x


fv :: Expr -> Set Name
fv (Var x) = one x
fv (Abs x s) = x `delete` fv s
fv (App s t) = fv s <> fv t


swp :: Name -> Name -> Expr -> Expr
swp a b (Var x)
  | x == a = Var b
  | x == b = Var a
  | otherwise = Var x
swp a b (Abs x s) = Abs x (swp a b s)
swp a b (App s t) = App (swp a b s) (swp a b t)


step :: Expr -> Expr
step (App (Abs x s) t) = subst x t s
step (App s t)
  | isVal s = App s (step t)
  | otherwise = App (step s) t
step x = x


isVal :: Expr -> Bool
isVal (Var _) = True
isVal _ = False


subst :: Name -> Expr -> Expr -> Expr
subst old new (Var x)
  | x == old = new
  | otherwise = Var x
subst old new (Abs x s) = Abs x' $ subst old new (swp x x' s)
  where
    x' = fresh x new
subst old new (App s t) = App (subst old new s) (subst old new t)
