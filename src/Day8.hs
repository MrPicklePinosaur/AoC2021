{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Text (Text, pack, unpack, splitOn)

solve :: IO ()
solve = do
  contents <- readFile "input/day8.txt"
  let parsed = parseInput $ lines contents
  print $ solveA parsed

solveA :: [([String], [String])] -> Int
solveA = sum . map solveLine
    where
        solveLine = length . filter (unique . length) . snd
        unique l = (l==2)||(l==3)||(l==4)||(l==7) -- i think this could be done better

parseInput :: [String] -> [([String], [String])]
parseInput = map $ parseLine "|"

parseLine :: String -> String -> ([String], [String])
parseLine char line = (left, right)
    where
        left = splitSide $ head lineSplit
        right = splitSide $ lineSplit!!1
        splitSide = filter (not . null) . map unpack . splitOn " "
        lineSplit = splitOn (pack char) $ pack line
