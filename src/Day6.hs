{-# LANGUAGE OverloadedStrings #-}

-- Solution Idea:
--
-- Have an array of buckets for fish of each counter from 0 to 8
-- Each day, to reduce the counter of each fish, just shift the entire array left
-- The number of fish of age 8 will be the number of fish at age 0
-- We also need to add in the fish that just reproduced back to bucket 6

module Day6 where

import qualified Data.Text as T

solve :: IO ()
solve = do
  contents <- readFile "input/day6.txt"
  let parsed = parseInput $ T.pack contents
  -- let test = [1, 2, 3, 4, 4, 4]
  -- print $ intoBuckets test
  -- print $ sim 5 test
  let buckets = intoBuckets parsed
  -- print $ solveA buckets
  print $ solveB buckets

-- output: 346063
solveA = sum . sim 80

solveB = sum . sim 256

parseInput :: T.Text -> [Int]
parseInput = map (read . T.unpack) . T.splitOn "," 

-- organize 
intoBuckets :: [Int] -> [Int]
intoBuckets input = map count [0..8]
    where
        count x = length $ filter (==x) input

sim :: Int -> [Int] -> [Int]
sim cycles = foldr (.) id (replicate cycles simStep)

simStep :: [Int] -> [Int]
simStep fish = (take 6 withBabies) ++ [withBabies!!6 + newFish] ++ (drop 7 withBabies)
    where
        withBabies = tail fish ++ [newFish] -- fish population with all fish aged and new fish included
        newFish = head fish -- number of fish ready to reproduce
