module Seats where

import Data.List (foldl', sortBy, sort)
import Data.Ord (comparing)
import System.IO
import Data.Monoid (mappend)

tupleSort (a1, a2) (b1, b2) = compare a1 b1 `mappend` compare a2 b2

rowCount = 128
colCount = 8

midpoint a b = div (a + b) 2

getRowOrCol rowString end' isRow = fst $ foldl' updateRange (0, end') rowString where
  updateRange (start, end) char
    | char == lowerChar = (start, midpoint start end)
    | char == upperChar = ((midpoint start end) + 1, end)
    | otherwise = (0,0)
  lowerChar = if isRow then 'F' else 'L'
  upperChar = if isRow then 'B' else 'R'

parseS s = (row, col) where
  row = getRowOrCol (take 7 s) (rowCount - 1) True
  col = getRowOrCol (drop 7 s) (colCount - 1) False

seatID (row, col) = row * 8 + col

main = do
  contents <- readFile "seats.txt"
  print $ head $ dropWhile (==0) $ zipWith (\a b -> if a /= b then a else 0) [11..] (sort $ map (seatID .parseS) $ lines contents) 

