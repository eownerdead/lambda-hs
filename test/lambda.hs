module Main where

import Lambda
import Relude
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "algorithm W" $ do
    it "id" $ infer (Let "id" (Abs "x" $ Var "x") (Var "id" `App` Unit))
      `shouldBe` TUnit
