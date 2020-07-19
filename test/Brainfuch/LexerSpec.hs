{-# LANGUAGE OverloadedStrings #-}

module Brainfuch.LexerSpec (spec) where

import Test.Hspec

import Brainfuch.Types
import Brainfuch.Lexer

allTokensInOneLine :: [Int] -> [Int] -> Either String [Token]
allTokensInOneLine l c = Right $
  zipWith ($)
    [
      Token NextData '>' 0
    , Token PrevData '<' 0
    , Token Increment '+' 0
    , Token Decrement '-' 0
    , Token Print '.' 0
    , Token Store ',' 0
    , Token JumpForth '[' 1
    , Token JumpBack ']' 1
    ]
    (zipWith Pos l c)

spec :: Spec
spec = do
  describe "Happy path" $
    it "all operators, no spaces" $
      mkTokens "><+-.,[]" `shouldBe` allTokensInOneLine (repeat 1) [1..8]

  describe "Spaces and newlines" $ do
    it "leading spaces are ignored" $
      mkTokens "   ><+-.,[]" `shouldBe` allTokensInOneLine (repeat 1) [4..11]

    it "trailing spaces are ignored" $
      mkTokens "><+-.,[]  " `shouldBe` allTokensInOneLine (repeat 1) [1..8]

    it "spaces inbetween operators are ignored" $
      mkTokens " > < + - . , [ ] " `shouldBe` allTokensInOneLine (repeat 1) (map (*2) [1..8])

    it "leading newlines are ignored" $
      mkTokens "\n><+-.,[]" `shouldBe` allTokensInOneLine (repeat 2) [1..8]

    it "trailing newlines are ignored" $
      mkTokens "><+-.,[]\n" `shouldBe` allTokensInOneLine (repeat 1) [1..8]

    it "newlines inbetween operators are ignored" $ -- because of the last (skip <|> eof) the trailing newline is not actually computed
      mkTokens "\n>\n<\n+\n-\n.\n,\n[\n]\n" `shouldBe` allTokensInOneLine [2..9] (repeat 1)

  describe "Depth index" $ do
    it "Simple loop: [+]" $
      mkTokens "-[+]-" `shouldBe` Right [
                                   Token Decrement '-' 0 $ Pos 1 1
                                 , Token JumpForth '[' 1 $ Pos 1 2
                                 , Token Increment '+' 1 $ Pos 1 3
                                 , Token JumpBack ']' 1 $ Pos 1 4
                                 , Token Decrement '-' 0 $ Pos 1 5]

    it "Nested loops: -[+[-]+]-" $
      mkTokens "-[+[-]+]-" `shouldBe` Right [
                                       Token Decrement '-' 0 $ Pos 1 1
                                     , Token JumpForth '[' 1 $ Pos 1 2
                                     , Token Increment '+' 1 $ Pos 1 3
                                     , Token JumpForth '[' 2 $ Pos 1 4
                                     , Token Decrement '-' 2 $ Pos 1 5
                                     , Token JumpBack ']' 2 $ Pos 1 6
                                     , Token Increment '+' 1 $ Pos 1 7
                                     , Token JumpBack ']' 1 $ Pos 1 8
                                     , Token Decrement '-' 0 $ Pos 1 9]

  describe "Unexpected characters cause error" $ do
    it "single unrecognized command, error" $
      mkTokens "a" `shouldBe` Left "Unexpected character a at (1,1)"

    it "leading external charachter, error" $
      mkTokens "a>" `shouldBe` Left "Unexpected character a at (1,1)"

    it "trailing external character, error" $
      mkTokens ">a" `shouldBe` Left "Unexpected character a at (1,2)"

    it "external character inbetween operators, error" $
      mkTokens ">a<" `shouldBe` (Left "Unexpected character a at (1,2)")

    it "parser stops at first unrecognized character" $
      mkTokens "abc" `shouldBe` Left "Unexpected character a at (1,1)"

  describe "Non matching brackets cause error" $ do
    it "single open bracket" $
      mkTokens "[" `shouldBe` Left "Non matching brackets"
    it "single closed bracket" $
      mkTokens "]" `shouldBe` Left "Non matching brackets"
    it "[[]" $
      mkTokens "[[]" `shouldBe` Left "Non matching brackets"
    it "[]]" $
      mkTokens "[]]" `shouldBe` Left "Non matching brackets"
    it "[[" $
      mkTokens "[[" `shouldBe` Left "Non matching brackets"
    it "]]" $
      mkTokens "]]" `shouldBe` Left "Non matching brackets"
    it "[]][" $
      mkTokens "[]][" `shouldBe` Left "Non matching brackets"
