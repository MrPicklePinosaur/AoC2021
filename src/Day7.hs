{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Text as T

solve :: IO ()
solve = do
  contents <- readFile "input/day7.txt"
  let parsed = parseInput $ T.pack contents
  print $ solveA parsed
  print $ solveB parsed

solveA = _solve cost
solveB = _solve costScaled

_solve :: (Int -> [Int] -> Int) -> [Int] -> Int
_solve costFn input = minimum $ map (\x -> costFn x input) [low..high]
    where
        high = maximum input
        low = minimum input 

cost :: Int -> [Int] -> Int
cost pos = sum . map (abs . (pos-))

costScaled :: Int -> [Int] -> Int
costScaled pos = sum . map (scale . abs . (pos-))
    where
        scale n = n*(n+1) `div` 2

parseInput :: T.Text -> [Int]
parseInput = map (read . T.unpack) . T.splitOn "," 

