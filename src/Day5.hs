{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.Text as T

-- solution idea: points intersect when they are not parallel, compare points that are horizontal and vertical mutually
-- TODO solution not efficient O(n^2)

data Line = Line
    { start :: (Int, Int)
    , end :: (Int, Int)
    } deriving (Show)

solve :: IO ()
solve = do
  contents <- readFile "input/day5.txt"
  let fileLines = lines contents
  let parsed = parseInput ([], []) fileLines
  print $ solveA parsed

solveA :: ([Line], [Line]) -> Int
solveA (hls, vls) = sum $ map countIntersect hls
  where
    countIntersect hl = length $ filter (isIntersectHV hl) vls
  
parseInput :: ([Line], [Line]) -> [String] -> ([Line], [Line])
parseInput acc [] = acc
parseInput (h, v) (x:xs)
  | isHorizontal line = parseInput (line : h, v) xs
  | isVertical line = parseInput (h, line : v) xs
  | otherwise = parseInput (h, v) xs
  where
    line = parseLine x

parseLine :: String -> Line
parseLine t = Line { start = toPoint start, end = toPoint end }
  where
    start = map (read . T.unpack) . T.splitOn "," $ splitted!!0
    end = map (read . T.unpack) . T.splitOn "," $ splitted!!1
    splitted = T.splitOn " -> " $ T.pack t

toPoint :: [Int] -> (Int, Int)
toPoint p = (p!!0, p!!1)

isHorizontal :: Line -> Bool
isHorizontal l = y1 l == y2 l

isVertical :: Line -> Bool
isVertical l = x1 l == x2 l

-- assumes that first line is horizontal and second is vertical
isIntersectHV :: Line -> Line -> Bool
isIntersectHV hl vl = x1 hl <= x1 vl && (x1 vl <= x2 hl)

x1 :: Line -> Int
x1 = fst . start

y1 :: Line -> Int
y1 = snd . start

x2 :: Line -> Int
x2 = fst . end

y2 :: Line -> Int
y2 = snd . end
