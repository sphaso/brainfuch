{-# LANGUAGE OverloadedStrings #-}

module Brainfuch.ParserSpec (spec) where

  import Test.Hspec

  import Brainfuch.Lexer
  import Brainfuch.Parser
  import Brainfuch.Types

  helperPlus :: Int -> Col -> Expression
  helperPlus d c = Plus $ Token Increment '+' d $ Pos 1 c

  spec :: Spec
  spec = do
    describe "Loops" $ do
      it "simple loop" $ do
        let Right code = mkTokens "+[+]+"
        mkProgram code `shouldBe` [helperPlus 0 1, Loop [helperPlus 1 3], helperPlus 0 5]
      it "nested loop, 1" $ do
        let Right code = mkTokens "+[+[+]+]+"
        mkProgram code `shouldBe` [helperPlus 0 1, Loop [helperPlus 1 3, Loop [helperPlus 2 5], helperPlus 1 7], helperPlus 0 9]
      it "nested loop, 2" $ do
        let Right code = mkTokens "+[+[+]+[+]+]+"
        mkProgram code `shouldBe` [helperPlus 0 1, Loop [helperPlus 1 3, Loop [helperPlus 2 5], helperPlus 1 7, Loop [helperPlus 2 9], helperPlus 1 11], helperPlus 0 13]

