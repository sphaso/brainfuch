{-# LANGUAGE FlexibleInstances #-}

module Brainfuch.Types where

  data Op = NextData
        | PrevData
        | Increment
        | Decrement
        | Print
        | Store
        | JumpForth
        | JumpBack
        deriving (Show, Eq)

  data Expression = MoveRight Token
                | MoveLeft Token
                | Plus Token
                | Minus Token
                | Out Token
                | In Token
                | Loop [Expression]
                | Stop
                deriving (Show, Eq)

  type Program = [Expression]

  type Col = Int
  type Row = Int
  data Pos = Pos Row Col deriving (Eq, Show)

  data Token = Token {op :: Op, token :: Char, depth :: Int, srcPos :: Pos} deriving (Eq, Show)

  instance Ord Pos where
    compare (Pos a b) (Pos i c) | a == i && b > c = GT
                                | a > i           = GT
                                | a == i && b < c = LT
                                | a < i           = LT
                                | True            = EQ

