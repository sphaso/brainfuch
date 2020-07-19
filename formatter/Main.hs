{-# LANGUAGE OverloadedStrings #-}

module Main where

  import qualified Data.Text as T
  import System.Environment (getArgs)

  import BrainfuchLib

  main :: IO ()
  main = do
    args <- getArgs
    case args of
      [] -> print "No path given"
      ["help"] -> print $ "Usage: formatter [FILE] | formatter -i [CODE]"
      ("-i":xs) -> print . format . T.pack . concat $ xs
      (path:_) -> do
          contents <- readFile path
          print $ format $ T.pack contents
