module Day3 where

import Data.Char (digitToInt)

-- APPROACH: sum all bits, if sum of all bits is greater than half of the total number of lines, then the most occuring bit in that bit position is a 1, otherwise it's zero

solve :: IO ()
solve = do
  contents <- readFile "input/day3.txt"
  let input = map parseLine $ lines contents
  print $ solveA input

parseLine = map digitToInt

solveA :: [[Int]] -> Int
solveA input = toDec bin * toDec (map not bin)
  where 
    bin = map (>halflen) sums
    sums = foldr1 (zipWith (+)) input
    halflen = length input `div` 2

toDec :: [Bool] -> Int
toDec = foldl (\acc x -> acc * 2 + fromEnum x) 0
