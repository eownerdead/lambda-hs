module Main where

import Relude
import Lambda.Lambda

main :: IO ()
main = print $ step $ App (Abs "x" (Abs "y" (Var "x"))) (Var "y")
