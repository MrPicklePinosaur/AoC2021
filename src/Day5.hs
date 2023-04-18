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
  let parsed = parseInput ([], [], []) fileLines
  print $ dpoints [Line { start = (9,7), end = (7,9) }]
  print $ dpoints [Line { start = (1,1), end = (3,3) }]
  -- print $ solveA parsed
  print $ solveB parsed

-- solveA :: ([Line], [Line]) -> Int
solveA (hls, vls, dls) = length $ filter (\(_, count) -> count > 1) counts
    where
        counts = occurences . concat $ hpoints ++ vpoints -- how many times each point is covered
        hpoints = map (\line -> [(x, y1 line) | x <- [x1 line..x2 line]]) hls -- all points that horizontal lines cover
        vpoints = map (\line -> [(x1 line, y) | y <- [y1 line..y2 line]]) vls -- all points that vertical lines cover

solveB (hls, vls, dls) = length $ filter (\(_, count) -> count > 1) counts
    where
        counts = occurences . concat $ hpoints ++ vpoints ++ dpoints dls-- how many times each point is covered
        hpoints = map (\line -> [(x, y1 line) | x <- [x1 line..x2 line]]) hls -- all points that horizontal lines cover
        vpoints = map (\line -> [(x1 line, y) | y <- [y1 line..y2 line]]) vls -- all points that vertical lines cover

dpoints = map (\line -> zip (smartRange (x1 line) (x2 line)) (smartRange (y1 line) (y2 line)))

smartRange e1 e2
    | e1 <= e2 = [e1..e2]
    | otherwise = reverse [e2..e1]

occurences :: Ord a => [a] -> [(a, Int)]
occurences xs = toList (fromListWith (+) [(x,1) | x <- xs])

parseInput :: ([Line], [Line], [Line]) -> [String] -> ([Line], [Line], [Line])
parseInput acc [] = acc
parseInput (h, v, d) (x:xs)
  | isHorizontal line = parseInput (line : h, v, d) xs
  | isVertical line = parseInput (h, line : v, d) xs
  | otherwise = parseInput (h, v, line : d) xs
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

x1 :: Line -> Int
x1 = fst . start

y1 :: Line -> Int
y1 = snd . start

x2 :: Line -> Int
x2 = fst . end

y2 :: Line -> Int
y2 = snd . end
