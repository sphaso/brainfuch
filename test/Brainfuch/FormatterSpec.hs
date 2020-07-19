{-# LANGUAGE OverloadedStrings #-}

module Brainfuch.FormatterSpec (spec) where

  import Data.Either (fromRight)
  import qualified Data.Text as T
  import Test.Hspec

  import Brainfuch.Lexer
  import Brainfuch.Formatter

  helpFormat :: T.Text -> T.Text
  helpFormat t = T.pack $ fromRight "fail" $ format 0 0 <$> mkTokens t

  spec :: Spec
  spec = do
    describe "Previous formatting is ignored" $ do
      it "spaces" $
        helpFormat " + > + < + > - , . " `shouldBe` "+>+<+>-,."
      it "tabs" $
        helpFormat "+\t-" `shouldBe` "+-"
      it "newlines" $
        helpFormat "+\n<>\n++\n" `shouldBe` "+<>++"
      it "both spaces and newlines" $
        helpFormat "+\n\t. ++ \n." `shouldBe` "+.++."

    describe "Grouping" $ do
      it "Few and sparse commands don't trigger formatting" $
        helpFormat "+>+<+>-,." `shouldBe` "+>+<+>-,."
      it "First group doesn't get a leading space" $
        helpFormat "+++++-" `shouldBe` "+++++ -"
      it "Single group doesn't trigger formatting" $
        helpFormat "+++++" `shouldBe` "+++++"
      it "Two groups" $
        helpFormat "+++++-,,,,,-" `shouldBe` "+++++ - ,,,,, -"

    describe "Loops" $ do
      it "One loop with one command" $
        helpFormat "[+]" `shouldBe` "[\n  +\n]"
      it "One loop wrapped by other commands" $
        helpFormat "-[+]-" `shouldBe` "- [\n  +\n]-"
      it "One loop with two groups" $
        helpFormat "-[+++++ ,,,,,]-" `shouldBe` "- [\n  +++++ ,,,,,\n]-"
      it "One nested loop" $
        helpFormat "-[+[><]+]-" `shouldBe` "- [\n  + [\n    ><\n  ]+\n]-"
      it "One nested loop with groups" $
        helpFormat "-[+++++[----->>>>>]-----]." `shouldBe` "- [\n  +++++ [\n    ----- >>>>>\n  ]-----\n]."

    describe "Idempotent" $ do
      mapM_ (\a -> it a $ ((helpFormat . helpFormat) $ T.pack a) `shouldBe` (helpFormat $ T.pack a))
        [
          "+>+<+>-,."
        , "++++-"
        , "+++++"
        , "+++++-,,,,,"
        , "[+]"
        , "-[+]-"
        , "-[+++++,,,,,]-"
        , "-[+[><]+]-"
        , "-[+++++[----->>>>>]-----]."
        ]

