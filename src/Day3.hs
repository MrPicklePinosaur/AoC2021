module Day3 where

import Data.Char (digitToInt)

-- APPROACH: sum all bits, if sum of all bits is greater than half of the total number of lines, then the most occuring bit in that bit position is a 1, otherwise it's zero

solve :: IO ()
solve = do
  contents <- readFile "input/day3_test.txt"
  let input = map parseLine $ lines contents
  -- print $ solveA input
  -- print $ lifeSupp input (>=) 0
  print $ solveB input

parseLine = map digitToInt

solveA :: [[Int]] -> Int
solveA input = toDec bin * toDec (map (1-) bin)
  where 
    bin = map (fromEnum . (>halflen)) sums
    sums = foldr1 (zipWith (+)) input
    halflen = length input `div` 2

solveB :: [[Int]] -> Int
solveB input = lifeSupp input (>=) 0 * lifeSupp input (<) 0
 
lifeSupp :: [[Int]] -> (Int -> Int -> Bool) -> Int -> Int
lifeSupp [] _ _ = 0
lifeSupp [x] _ _ = toDec x
lifeSupp input cmp i = lifeSupp (filter (\a -> a!!i==freq) input) cmp nextInd
  where
    nextInd = i+1 `mod` length (head input)
    freq = fromEnum $ cmp colsum halflen
    colsum = foldr (\x acc -> acc + (x!!i)) 0 input
    halflen = length input `div` 2

toDec :: [Int] -> Int
toDec = foldl (\acc x -> acc * 2 + x) 0
