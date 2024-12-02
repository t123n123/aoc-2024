#!/usr/bin/env runhaskell

import Data.Fixed (E0)
import Data.List
import System.IO

mono xs = (length . nub) (zipWith (\x y -> signum (x - y)) xs (drop 1 xs)) == 1

dsf xs = (and (zipWith (\x y -> (1 <= abs (x - y) && abs (x - y) <= 3)) xs (drop 1 xs)))

partOne xs = (filter (\ls -> mono ls && dsf ls)) $ xs

partTwo :: [[Int]] -> Int
partTwo xs = length . filter (\ls -> not . null . partOne $ zipWith (\x y -> (x ++ (drop 1 y))) (inits ls) (tails ls)) $ xs

main = do
  contents <- readFile "input"
  print . length . partOne . map (map read . words) . lines $ contents
  print . partTwo . map (map read . words) . lines $ contents
