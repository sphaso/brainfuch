module Brainfuch.Interpreter (Environment (..), evaluate, initEnvironment) where

  import Data.List
  import qualified Data.Map.Strict as M
  import Data.Maybe (fromMaybe, Maybe (..))

  import Brainfuch.Types

  type Data = (Int, M.Map Int Int)
  data Environment = Environment {nextExpressions :: Program, contextProgram :: Program, db :: Data, userInput :: [Int], toPrintRev :: [Int], inLoop :: Bool} deriving (Eq, Show)

  initEnvironment :: Program -> [Int] -> Environment
  initEnvironment p i = Environment p p (0, M.singleton 0 0) i [] False

  modifyData :: Data -> (Maybe Int -> Maybe Int) -> Data
  modifyData (k, m) f = (k, M.alter f k m)

  evaluate :: Environment -> Environment
  evaluate e@(Environment [] _ _ _ _ False) = e
  evaluate e@(Environment [] p (k, storage) _ _ True) =
    if M.lookup k storage /= Just 0 then
      evaluate $ e {nextExpressions = p}
    else
      e
  evaluate e@(Environment (MoveRight _:xs) _ (k, storage) _ _ _) =
    evaluate $ e {nextExpressions = xs, db = (k + 1, storage)}
  evaluate e@(Environment (MoveLeft _:xs) _ (k, storage) _ _ _) =
    evaluate $ e {nextExpressions = xs, db = (k - 1, storage)}
  evaluate e@(Environment (Plus _:xs) _ kdata _ _ _) =
    evaluate $ e {nextExpressions = xs, db = modifyData kdata (Just . maybe 1 succ)}
  evaluate e@(Environment (Minus _:xs) _ kdata _ _ _) =
    evaluate $ e {nextExpressions = xs, db = modifyData kdata (Just . maybe (-1) pred)}
  evaluate e@(Environment (Out _:xs) _ (k, storage) _ tp _) =
    let output = M.lookup k storage
    in evaluate $ e {nextExpressions = xs, toPrintRev = (fromMaybe 0 output):tp}
  evaluate e@(Environment (In _:xs) _ kdata (i:ix) tp _) =
    evaluate $ e {nextExpressions = xs, db = modifyData kdata (const $ Just i), userInput = ix}
  evaluate e@(Environment (Loop exps:xs) p (k, storage) i tp il) =
    if M.lookup k storage == Just 0 then
      evaluate $ e {nextExpressions = xs}
    else
      let inner = evaluate $ Environment exps exps (k, storage) i tp True
      in evaluate $ inner {nextExpressions = xs, contextProgram = p, inLoop = il}

