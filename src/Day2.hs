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
  print $ solveA input
  print $ solveB input

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

solveA :: [Command] -> Int
solveA input = positionA input 0 0
        
positionA :: [Command] -> Int -> Int -> Int
positionA [] x y = x*y
positionA c x y = case head c of
  Up v -> positionA (tail c) x (y-v)
  Down v -> positionA (tail c) x (y+v)
  Forward v -> positionA (tail c) (x+v) y

solveB :: [Command] -> Int
solveB input = positionB input 0 0 0
        
positionB :: [Command] -> Int -> Int -> Int -> Int
positionB [] x y a = x*y
positionB c x y a = case head c of
  Up v -> positionB (tail c) x y (a-v)
  Down v -> positionB (tail c) x y (a+v)
  Forward v -> positionB (tail c) (x+v) (y+a*v) a
