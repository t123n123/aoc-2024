import Data.Map (Map, elems, empty, foldrWithKey, keys, mapWithKey, singleton, unionWith)
import Data.Map qualified (map)

main = do
  input <- map readI <$> words <$> readFile "input"
  test <- map readI <$> words <$> readFile "test_input"
  let problem = input

  print $ sum $ elems $ blinkCountTimes 25 (foldl (unionWith (+)) empty (map (\k -> singleton k 1) problem))
  print $ sum $ elems $ blinkCountTimes 75 (foldl (unionWith (+)) empty (map (\k -> singleton k 1) problem))

readI = read :: String -> Int

blinkCountTimes :: Int -> Map Int Int -> Map Int Int
blinkCountTimes 0 = id
blinkCountTimes n = (foldrWithKey f empty) . (blinkCountTimes (n - 1))
  where
    f key value mp = unionWith (+) mp (Data.Map.map (* value) (blinkCount key))

blinkCount :: Int -> Map Int Int
blinkCount n
  | n == 0 = singleton 1 1
  | digits `mod` 2 == 0 = unionWith (+) first second
  | otherwise = singleton (n * 2024) 1
  where
    digits = 1 + (floor $ logBase 10 (fromIntegral n))
    digits_half_power = 10 ^ (digits `div` 2)
    first = singleton (n `div` digits_half_power) 1
    second = singleton (n `mod` digits_half_power) 1
