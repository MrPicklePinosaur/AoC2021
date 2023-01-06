{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe (isJust, fromJust)

data Board = Board
    { rows :: [[Int]]
    , columns :: [[Int]]
    } deriving (Show)

solve :: IO ()
solve = do
  contents <- readFile "input/day4.txt"
  let fileLines = lines contents

  let moves = parseMoves $ T.pack $ head fileLines
  let rawBoards = readBoards 6 $ tail fileLines
  let parsedBoards = (map.map) (parseRow . T.pack) rawBoards
  let boards = map createBoard parsedBoards
  print $ solveA moves boards
  print $ solveB moves boards

solveA :: [Int] -> [Board] -> Int
solveA [] _ = 0
solveA (x:xs) b = case winnerBoard of
  Just board -> calculateScore x $ fromJust winnerBoard
  Nothing -> solveA xs stepped
  where
    winnerBoard = L.find isWinner stepped
    stepped = map (stepBoard x) b

solveB :: [Int] -> [Board] -> Int
solveB [] _ = 0
solveB (x:xs) b
  | L.null next && not (L.null winners) = calculateScore x $ head winners
  | otherwise = solveB xs next
  where
    winners = filter isWinner stepped
    next = filter (not . isWinner) stepped
    stepped = map (stepBoard x) b


calculateScore :: Int -> Board -> Int
calculateScore i b = i * (sum . map sum $ rows b)

parseMoves :: T.Text -> [Int]
parseMoves = map (read . T.unpack) . T.splitOn ","

readBoards :: Int -> [a] -> [[a]]
readBoards _ [] = []
readBoards n l = tail (take n l) : readBoards n (drop n l)

createBoard :: [[Int]] -> Board
createBoard rows = Board { rows = rows, columns = columns }
  where
    columns = L.transpose . L.reverse $ rows

parseRow :: T.Text-> [Int]
parseRow t =
  let filtered = filter (not . T.null) $ T.splitOn " " t
  in map (read . T.unpack) filtered

-- crosses out number on all rows and columns of board
stepBoard :: Int -> Board -> Board
stepBoard i b = Board { rows = rows', columns = columns' }
  where
    rows' = map (filter (/=i)) $ rows b
    columns' = map (filter (/=i)) $ columns b

isWinner :: Board -> Bool
isWinner b = rowWinner || colWinner
  where
    rowWinner = isJust . L.find L.null $ rows b
    colWinner = isJust . L.find L.null $ columns b
    
