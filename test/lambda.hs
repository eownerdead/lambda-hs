module Main where

import Lambda
import Relude
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "step" $ do
    it "name conflict" $ step (App (Abs "x" (Abs "y" (Var "x"))) (Var "y"))
      `shouldBe` Abs "y'" (Var "y")

    it "no free vars" $ step (App (Abs "x" (Abs "y" (Var "y"))) (Var "y"))
      `shouldBe` Abs "y'" (Var "y'")
