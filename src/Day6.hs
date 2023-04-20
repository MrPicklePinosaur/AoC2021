{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import qualified Data.Text as T

solve :: IO ()
solve = do
  contents <- readFile "input/day6.txt"
  let parsed = parseInput $ T.pack contents
  -- let test = [1, 2, 3, 4]
  -- print $ sim 5 test
  print . length $ sim 80 parsed

parseInput :: T.Text -> [Int]
parseInput = map (read . T.unpack) . T.splitOn "," 

sim :: Int -> [Int] -> [Int]
sim cycles = foldr (.) id (replicate cycles simStep)

simStep :: [Int] -> [Int]
simStep = foldr simFish []

simFish :: Int -> [Int] -> [Int]
simFish age acc
  | age > 0 = acc++[age-1]
  | otherwise = acc++[6,8]
