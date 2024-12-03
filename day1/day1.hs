#!/usr/bin/env runhaskell

import Data.List
import Data.Set (fromList, member)
import System.IO

splitLists [] = ([], [])
splitLists (x : y : xs) = (x : fs, y : ss)
 where
  (fs, ss) = splitLists xs

partOne xs = sum $ zipWith (\x y -> abs (x - y)) (sort as) (sort bs)
 where
  (as, bs) = splitLists xs

partTwo xs = sum $ filter (\x -> member x fs) bs
 where
  (as, bs) = splitLists xs
  fs = fromList as

main = do
  contents <- readFile "input"
  print . partOne . map read . words $ contents
  print . partTwo . map read . words $ contents
