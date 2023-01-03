{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T
import Data.Text.Read
import Data.Either

data Command = Up Int | Down Int | Forward Int deriving (Show)

solve :: IO ()
solve = do
  contents <- readFile "input/day2.txt"
  let input = map parseLine $ lines contents
  print $ position input 0 0
  
parseLine :: String -> Command
parseLine l = parse (T.splitOn " " (T.pack l))

parse :: [T.Text] -> Command
parse [c,v] = command
  where
    command = case c of
        "up" -> Up value
        "down" -> Down value
        "forward" -> Forward value
    value = read $ T.unpack v
        
position :: [Command] -> Int -> Int -> Int
position [] x y = x*y
position c x y = case head c of
  Up v -> position (tail c) x (y-v)
  Down v -> position (tail c) x (y+v)
  Forward v -> position (tail c) (x+v) y

