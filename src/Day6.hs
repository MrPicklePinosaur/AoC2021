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
simStep fish = agedFish ++ newFish
    where
        newFish = replicate newFishCount 8
        newFishCount = length $ filter (==0) fish
        agedFish = map simFish fish
        simFish age
          | age > 0 = age-1
          | otherwise = 6
