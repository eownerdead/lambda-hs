module Main where

import Lambda
import Relude


main :: IO ()
main = print $ step $ App (Abs "x" (Abs "y" (Var "x"))) (Var "y")
