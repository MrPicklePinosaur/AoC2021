{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Text (Text, pack, unpack, splitOn)
import Data.List
import Data.Maybe (fromJust)

solve :: IO ()
solve = do
  contents <- readFile "input/day8.txt"
  let parsed = parseInput $ lines contents
  -- print $ intersectDigits ["ab", "abd"]
  -- print $ subtractDigits "abc" "dab"
  -- print $ containsDigits "abcd" "ace"
  -- print . solveLine $ head parsed
  -- print $ solveA parsed
  print $ solveB parsed


solveA :: [([String], [String])] -> Int
solveA = sum . map solveLine
    where
        solveLine = length . filter (unique . length) . snd
        unique l = (l==2)||(l==3)||(l==4)||(l==7) -- i think this could be done better

solveB :: [([String], [String])] -> Int
solveB = sum . map solveLine

solveLine :: ([String], [String]) -> Int
solveLine (calib, output) = foldl (\acc x -> 10*acc + x) 0 $ map (\x -> fromJust $ x `elemIndex` decoded) $ map sort output
    where
        decoded = decodeLine calib

-- determine what each symbol represents
-- TODO this function looks HORRIBLE
decodeLine :: [String] -> [String]
decodeLine calib = map sort [rrr,one_gad,ll,lrr,four_gad,lrl,rrl,seven_gad,eight_gad,rr]
    where
        bd_gad = four_gad `subtractDigits` one_gad
        eg_gad = eight_gad `subtractDigits` seven_gad `subtractDigits` four_gad
        one_gad = findWithSegments 2 calib -- 1
        four_gad = findWithSegments 4 calib -- 4
        seven_gad = findWithSegments 3 calib -- 7
        eight_gad = findWithSegments 7 calib -- 8

        l = filter ((==5) . length) calib
        r = filter ((==6) . length) calib
        ll = head $ filter (`containsDigits` eg_gad) l -- 2
        lr = filter (\x -> not $ x `containsDigits` eg_gad) l
        lrl = head $ filter (`containsDigits` bd_gad) lr -- 5
        lrr = head $ filter (\x -> not $ x `containsDigits` bd_gad) lr -- 3

        rl = filter (`containsDigits` eg_gad) r
        rr = head $ filter (\x -> not $ x `containsDigits` eg_gad) r -- 9
        rrl = head $ filter (`containsDigits` bd_gad) rl -- 6
        rrr = head $ filter (\x -> not $ x `containsDigits` bd_gad) rl -- 0

parseInput :: [String] -> [([String], [String])]
parseInput = map $ parseLine "|"

parseLine :: String -> String -> ([String], [String])
parseLine char line = (left, right)
    where
        left = splitSide $ head lineSplit
        right = splitSide $ lineSplit!!1
        splitSide = filter (not . null) . map unpack . splitOn " "
        lineSplit = splitOn (pack char) $ pack line

-- Look for first digit with given number of segments
findWithSegments :: Int -> [String] -> String
findWithSegments l = fromJust . find ((==l) . length)

-- intersection of characters in string
intersectDigits :: [String] -> String
intersectDigits = foldl intersect "abcdef"

subtractDigits :: String -> String -> String
subtractDigits a b = filter (`notElem` b) a

containsDigits :: String -> String -> Bool
containsDigits str = foldr ((&&) . (`elem` str)) True

