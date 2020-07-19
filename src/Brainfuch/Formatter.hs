module Brainfuch.Formatter (format) where

  import Brainfuch.Types

-- Rules:
-- '[' introduces a newline and a 2 characters indent increase
-- ']' introduces a newline and a 2 characters indent decrease
-- 5 or more consecutive characters (except []) introduce a space around it
-- break a line after 15 commands

  type Indent = Int
  type Commands = Int

  groupTokens :: [Token] -> (String, [Token])
  groupTokens [] = ("", [])
  groupTokens l@(x:xs) =
      let
        group = takeWhile ((== op x) . op) l
      in
        if length group >= 5 then
          (replicate (length group) $ token x, dropWhile ((== op x) . op) xs)
        else
          (token x:[], xs)

  format :: Indent -> Commands -> [Token] -> String
  format _ _ [] = ""
  format i c l@(x:xs)
    | op x == JumpForth = indent <> [token x] <> "\n" <> space incr <> format incr 0 xs
    | op x == JumpBack = "\n" <> space decr <> [token x] <> break <> format decr 0 xs
    | True = wrap <> break <> format i newC r
    where
      space n = replicate n ' '
      incr = i + 2
      decr = i - 2
      (g, r) = groupTokens l
      indent = if c == 0 then space i else " "
      leadingSpace = if c == 0 then "" else " "
      trailingSpace = if length g > 0 && length r > 0 && (op $ head r) /= JumpBack && ((fst $ groupTokens r) `elem` ["+", "-", ">", "<", ".", ","]) then " " else ""
      wrap = if length g > 1 then
               leadingSpace <> g <> trailingSpace
             else
               g
      break = if c > 0 && length g > 15 && length r > 0 then "\n" else ""
      newC = if break == "\n" then 0 else c + length g

