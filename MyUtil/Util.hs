module Util where

readInt :: String -> Int
readInt = read

positionList :: [[a]] -> [(Int, Int, a)]
positionList xs = positionList' 0 0 xs
  where
    positionList' x y ([] : xs) = positionList' 0 (y + 1) xs
    positionList' x y ((a : xs) : ys) = (x, y, a) : positionList' (x + 1) y (xs : ys)
    positionList' x y [] = []
