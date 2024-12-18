module Util where

import Data.List

readInt :: String -> Int
readInt = read

positionList :: [[a]] -> [[(Int, Int, a)]]
positionList xs = positionList' 0 0 xs

positionList' x y ([] : xs) = positionList' 0 (y + 1) xs
positionList' x y ((a : xs) : ys) = (((x, y, a) : xs') : ys')
  where
    (xs' : ys') = positionList' (x + 1) y (xs : ys)
positionList' x y [] = []

gridwalk :: [[a]] -> ([[a]] -> b) -> [[b]]
gridwalk xs f = [[f . transpose $ y | y <- tails . transpose $ x] | x <- tails xs]

pad :: [[a]] -> a -> [[a]]
pad xs v = [replicate (2 + (length $ head xs)) v] ++ (map (\l -> [v] ++ l ++ [v]) xs) ++ [replicate (2 + (length $ head xs)) v]
