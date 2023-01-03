module Day1 where

solve :: IO ()
solve = do
  contents <- readFile "input/day1.txt"
  let input = map readInt . lines $ contents
  print . solveA $ input

solveA :: [Int] -> Int
solveA input =  length . filter (>0) $ diff input

readInt :: String -> Int
readInt = read

diff :: [Int] -> [Int]
diff l = zipWith (-) (tail l) l
