{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Text as T

solve :: IO ()
solve = do
  contents <- readFile "input/day7.txt"
  let parsed = parseInput $ T.pack contents
  print $ solveA parsed

solveA :: [Int] -> Int
solveA input = minimum $ map (\x -> cost x input) [low..high]
    where
        high = maximum input
        low = minimum input 

cost :: Int -> [Int] -> Int
cost pos = sum . map (abs . (pos-))

parseInput :: T.Text -> [Int]
parseInput = map (read . T.unpack) . T.splitOn "," 

