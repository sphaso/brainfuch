{-# LANGUAGE OverloadedStrings #-}

module Brainfuch.InterpreterSpec (spec) where

  import Test.Hspec
  import Data.Char (chr)
  import qualified Data.Map.Strict as M

  import Brainfuch.Types
  import Brainfuch.Parser
  import Brainfuch.Lexer
  import Brainfuch.Interpreter

  gimmeEnv t e =
    let
      Right tokens = mkTokens t
      prog = mkProgram tokens
    in
      evaluate $ initEnvironment prog e

  gimmeDb :: Environment -> [Int]
  gimmeDb = map snd . M.toAscList . snd . db

  spec :: Spec
  spec = do
    describe "Single operations" $ do
      it ">" $ do
        let res = gimmeEnv ">" []
        (gimmeDb res, toPrintRev res) `shouldBe` ([0], [])
      it "<" $ do
        let res = gimmeEnv "<" []
        (gimmeDb res, toPrintRev res) `shouldBe` ([0], [])
      it "+" $ do
        let res = gimmeEnv "+" []
        (gimmeDb res, toPrintRev res) `shouldBe` ([1], [])
      it "-" $ do
        let res = gimmeEnv "-" []
        (gimmeDb res, toPrintRev res) `shouldBe` ([-1], [])
      it "." $ do
        let res = gimmeEnv "." []
        (gimmeDb res, toPrintRev res) `shouldBe` ([0], [0])
      it "," $ do
        let res = gimmeEnv "," [1]
        (gimmeDb res, toPrintRev res) `shouldBe` ([1], [])
      it "[]" $ do
        let res = gimmeEnv "[]" []
        (gimmeDb res, toPrintRev res) `shouldBe` ([0], [])

    describe "Simple combinations" $ do
      it "+++>++>+ = 3, 2, 1" $ do
        let res = gimmeEnv "+++>++>+" []
        (gimmeDb res, toPrintRev res) `shouldBe` ([3, 2, 1], [])
      it "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]" $ do
        let res = gimmeEnv "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]" []
        (gimmeDb res, toPrintRev res) `shouldBe` ([0, 0, 72, 104, 88, 32, 8], [])

    describe "Programs" $ do
      it "Sum, 2+5=7" $ do
        let res = gimmeEnv "++>+++++ [<+>-]" []
        (gimmeDb res, toPrintRev res) `shouldBe` ([7, 0], [])
      it "Hello world!" $ do
        let res = gimmeEnv "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." []
        (map chr $ reverse $ toPrintRev res) `shouldBe` "Hello World!\n"
