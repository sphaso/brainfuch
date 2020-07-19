{-# LANGUAGE OverloadedStrings #-}

module BrainfuchLibSpec (spec) where

  import Test.Hspec
  import Data.Either (isRight)

  import BrainfuchLib

  spec :: Spec
  spec = describe "BrainfuchLib" $ do
           describe "execute" $ do
             it "Wrong code returns the parser error" $
               execute "+a+" [] `shouldBe` Left "Unexpected character a at (1,2)"
             it "Right code returns the environment" $
               execute "++" [] `shouldSatisfy` isRight

           describe "prettyPrintMemory" $ do
             it "One cell" $ do
               let Right env = execute "++" []
               prettyPrintMemory env `shouldBe` "0 | 2\n"
             it "Skip empty cells" $ do
               let Right env = execute "+>>+" []
               prettyPrintMemory env `shouldBe` "0 | 1\n2 | 1\n"
             it "Skip first cell" $ do
               let Right env = execute ">>+" []
               prettyPrintMemory env `shouldBe` "0 | 0\n2 | 1\n"
             it "Padding words" $ do
               let Right env = execute "+>>>>>>>>>>+" []
               prettyPrintMemory env `shouldBe` "0  | 1\n10 | 1\n"
