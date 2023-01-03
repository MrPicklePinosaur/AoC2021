module Day1 where

import Data.List (tails)

solve :: IO ()
solve = do
  contents <- readFile "input/day1.txt"
  let input = map readInt . lines $ contents
  print . solveA $ input
  print . solveB $ input

solveA :: [Int] -> Int
solveA input = length . filter (>0) $ diff input

solveB :: [Int] -> Int
solveB input = length . filter (>0) $ diff sums
    where sums = map sum $ windows 3 input

windows' :: Int -> [a] -> [[a]]
windows' n = map (take n) . tails

takeLengthOf :: [a] -> [b] -> [b]
takeLengthOf = zipWith (flip const)

windows :: Int -> [a] -> [[a]]
windows n xs = takeLengthOf (drop (n-1) xs) (windows' n xs)

readInt :: String -> Int
readInt = read

diff :: [Int] -> [Int]
diff l = zipWith (-) (tail l) l
