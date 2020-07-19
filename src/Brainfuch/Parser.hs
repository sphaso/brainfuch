module Brainfuch.Parser (mkProgram) where

  import Brainfuch.Types

  tokenToExpression :: Token -> Expression
  tokenToExpression t@Token {op = NextData}  = MoveRight t
  tokenToExpression t@Token {op = PrevData}  = MoveLeft t
  tokenToExpression t@Token {op = Increment} = Plus t
  tokenToExpression t@Token {op = Decrement} = Minus t
  tokenToExpression t@Token {op = Print}     = Out t
  tokenToExpression t@Token {op = Store}     = In t

  mkProgram :: [Token] -> Program
  mkProgram [] = []
  mkProgram (Token {op = JumpForth, depth = d}:xs) =
    let
      inner = mkProgram xs
      rest = dropWhile ((>=d) . depth) xs
    in (Loop inner) : mkProgram rest
  mkProgram (Token {op = JumpBack}:xs) = []
  mkProgram (t:xs) = tokenToExpression t : mkProgram xs
