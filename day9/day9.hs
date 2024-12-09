import Data.List (find, partition)
import Foreign (newArray0)

main = do
  input <- concatMap readDigit <$> readFile "input"
  test <- concatMap readDigit <$> readFile "test_input"
  let problem = input
      all = [(i, f) | (i, f) <- zip [0 ..] problem]
      files_total_size = sum [f | (i, f) <- all, even i]
      expanded = concat [replicate f i | (i, f) <- all]
      result = solve expanded (reverse expanded)
      checksum = sum [(i * f) `div` 2 | (i, f) <- zip [0 ..] (take files_total_size result)]

      result' = solve' all (reverse all)
      checksum' = calc_checksum' 0 result'

  print checksum
  print checksum'

solve' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
solve' [] [] = []
solve' xs [] = xs
solve' [] ys = ys
solve' xs ((yx, yy) : ys)
  | odd yx = solve' xs ys
  | otherwise = solve' (fill [] xs (yx, yy)) ys
  where
    fill acc ((c, d) : xs) (a, b)
      | (c, d) == (a, b) = (reverse acc) ++ ((a, b) : xs)
      | odd c && d == b = (reverse acc) ++ ((a, b) : make_empty (a, b) xs)
      | odd c && d > b = (reverse acc) ++ [(a, b), (c, d - b)] ++ (make_empty (a, b) xs)
      | otherwise = fill ((c, d) : acc) xs (a, b)

make_empty (a, b) [] = []
make_empty (a, b) ((c, d) : xs)
  | (a, b) == (c, d) = (0, b) : xs
  | otherwise = (c, d) : (make_empty (a, b) xs)

calc_checksum' idx [] = 0
calc_checksum' idx ((a, b) : xs)
  | even a = ((b * idx + ((b - 1) * b `div` 2)) * a `div` 2) + calc_checksum' (idx + b) xs
  | otherwise = calc_checksum' (idx + b) xs

readDigit :: Char -> [Int]
readDigit '\n' = []
readDigit c = [read [c]]

solve :: [Int] -> [Int] -> [Int]
solve [] [] = []
solve xs [] = xs
solve [] ys = ys
solve (x : xs) (y : ys)
  | even x = x : (solve xs (y : ys))
  | even y = y : (solve xs ys)
  | otherwise = solve (x : xs) ys
