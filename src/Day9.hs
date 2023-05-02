{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import Data.Text (Text, pack, unpack, splitOn)
import Data.List as L
import Data.Maybe (fromJust, catMaybes)
import Data.Char (digitToInt)
import Data.Map (fromList, member)
import Data.Map as M
import Data.Bifunctor (bimap)

type Grid = M.Map (Int, Int) Int

solve :: IO ()
solve = do
  -- contents <- readFile "input/day9_test.txt"
  contents <- readFile "input/day9.txt"
  let gridMap = parseInput $ lines contents
  -- print $ M.lookup (0,0) gridMap
  -- print $ minCell gridMap (3,2)
  -- print $ minCell gridMap (3,3)
  print $ solveA gridMap

solveA :: Grid -> Int
solveA grid = sum $ L.map (\x -> minCell grid $ fst x) cells
    where
        cells = toList grid

-- Check if a given cell is the lowest cell of all neighbouring cells
-- returns the cell value if it is min cell, otherwise zero
minCell :: Grid -> (Int, Int) -> Int
minCell grid cell = if all (>val) around then val+1 else 0
    where
        val = fromJust $ M.lookup cell grid
        around = neighbour grid cell

-- Get the four neighbours of a given cell
neighbour :: Grid -> (Int, Int) -> [Int]
neighbour grid cell = catMaybes $ L.map (\c -> M.lookup c grid) around
    where
        around = L.map (tupleAdd cell) [(0, 1), (0, -1), (1, 0), (-1, 0)]

parseInput :: [String] -> Grid
parseInput lines = fromList $ concat intGrid
    where
        intGrid = L.map (\(row, line) -> zip [( col, row ) | col <- [0..]] $ L.map digitToInt line) rows
        rows = zip [0..] lines

tupleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleAdd (a, b) (c, d) = (a+c, b+d)
