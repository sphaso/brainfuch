module BrainfuchLib where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Brainfuch.Formatter as Formatter
import Brainfuch.Interpreter
import Brainfuch.Lexer
import Brainfuch.Parser
import Brainfuch.Types

execute :: T.Text -> [Int] -> Either String Environment
execute src input = evaluate . (flip initEnvironment input) . mkProgram <$> mkTokens src

format :: T.Text -> String
format t = either id id (Formatter.format 0 0 <$> mkTokens t)

prettyPrintMemory :: Environment -> String
prettyPrintMemory Environment {db = (_, db)} =
  let
    m = M.toAscList db
    max' = maximum $ map fst m
    indent = succ $ max' `div` 10
    padLeft s = if length s < indent then
                  s ++ replicate (indent - length s) ' '
                else
                  s
  in
    unlines $ map (\(a, b) -> (padLeft $ show a) ++ " | " ++ show b) m

getOutput :: Environment -> String
getOutput = show . reverse . toPrintRev
