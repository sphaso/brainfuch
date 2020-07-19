{-# LANGUAGE OverloadedStrings #-}

module Brainfuch.Lexer (mkTokens) where
  import Brainfuch.Types

  import qualified Data.Text as T
  import Text.Parsec.Pos (sourceLine, sourceColumn)
  import Text.Parsec.Error (messageString, errorMessages)
  import Text.Parsec.Text (GenParser)
  import Text.Parsec.Combinator
  import Text.Parsec.Prim
  import Text.Parsec.Char (oneOf, space, newline, anyChar)

  type Parser = GenParser Int


  operatorMap :: Char -> Op
  operatorMap '>' = NextData
  operatorMap '<' = PrevData
  operatorMap '+' = Increment
  operatorMap '-' = Decrement
  operatorMap '.' = Print
  operatorMap ',' = Store
  operatorMap '[' = JumpForth
  operatorMap ']' = JumpBack

  operator :: Parser Token
  operator = do
    op <- oneOf "><+-.,[]"
    ix <- getState
    let (tDepth, newS) = case op of
                           '[' -> (ix + 1, ix + 1)
                           ']' -> (ix, ix - 1)
                           _   -> (ix, ix)
    if (newS < 0) then
      parserFail "Non matching brackets"
    else do
      putState newS
      srcPos <- getPosition
      return $ Token (operatorMap op) op tDepth $ Pos (sourceLine srcPos) (sourceColumn srcPos - 1)

  invalidCharacter :: Parser Token
  invalidCharacter = do
    x <- anyChar
    srcPos <- getPosition
    unexpected $ "Unexpected character " <> [x] <> " at " <> show (sourceLine srcPos, sourceColumn srcPos - 1)

  skip :: Parser ()
  skip = optional $ skipMany $ space <|> newline

  brainCode :: Parser [Token]
  brainCode = do
    res <- many1 $ skip *> (operator <|> invalidCharacter) <* (skip <|> eof)
    i <- getState
    if i /= 0 then
      parserFail "Non matching brackets"
    else
      return res

  mkTokens :: T.Text -> Either String [Token]
  mkTokens src = case runParser brainCode 0 "brainfuch" src of
                   Left err -> Left $ head $ reverse $ map messageString $ errorMessages err
                   Right tokens -> Right tokens
