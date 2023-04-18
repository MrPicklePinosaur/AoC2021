{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.Text as T
import Data.Map (fromListWith, toList)

-- solution idea: points intersect when they are not parallel, compare points that are horizontal and vertical mutually
-- TODO solution not efficient O(n^2)
-- ISSUE does not take into account overlapping lines :(

type Point = (Int, Int)
  
data Line = Line
    { start :: Point
    , end :: Point
    } deriving (Show)

solve :: IO ()
solve = do
  contents <- readFile "input/day5.txt"
  let fileLines = lines contents
  let parsed = parseInput ([], []) fileLines
  -- let test = solveA ([Line { start = (0,0), end = (5, 0) }, Line { start = (0, 7), end = (2, 7) }], [ Line { start = (0, 0), end = (0, 5) } ])
  print $ solveA parsed

-- solveA :: ([Line], [Line]) -> Int
solveA (hls, vls) = length $ filter (\(_, count) -> count > 1) counts
    where
        counts = occurences . concat $ hpoints ++ vpoints -- how many times each point is covered
        hpoints = map (\line -> [(x, y1 line) | x <- [x1 line..x2 line]]) hls -- all points that horizontal lines cover
        vpoints = map (\line -> [(x1 line, y) | y <- [y1 line..y2 line]]) vls -- all points that vertical lines cover

occurences :: Ord a => [a] -> [(a, Int)]
occurences xs = toList (fromListWith (+) [(x,1) | x <- xs])

parseInput :: ([Line], [Line]) -> [String] -> ([Line], [Line])
parseInput acc [] = acc
parseInput (h, v) (x:xs)
  | isHorizontal line = parseInput (line : h, v) xs
  | isVertical line = parseInput (h, line : v) xs
  | otherwise = parseInput (h, v) xs
  where
    line = parseLine x

parseLine :: String -> Line
parseLine t
  | start < end = Line { start = start, end = end }
  | otherwise = Line { start = end, end = start }
  where
    start = parse $ splitted!!0
    end = parse $ splitted!!1
    parse = toPoint . map (read . T.unpack) . T.splitOn ","
    splitted = T.splitOn " -> " $ T.pack t

toPoint :: [Int] -> Point
toPoint p = (p!!0, p!!1)

isHorizontal :: Line -> Bool
isHorizontal l = y1 l == y2 l

isVertical :: Line -> Bool
isVertical l = x1 l == x2 l

-- assumes that first line is horizontal and second is vertical
isIntersectHV :: Line -> Line -> Bool
isIntersectHV hl vl = x1 hl <= x1 vl && (x1 vl <= x2 hl) && (y1 vl <= y1 hl) && (y1 hl <= y2 vl) 

x1 :: Line -> Int
x1 = fst . start

y1 :: Line -> Int
y1 = snd . start

x2 :: Line -> Int
x2 = fst . end

y2 :: Line -> Int
y2 = snd . end
