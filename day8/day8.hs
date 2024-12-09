import Data.List (find, group, sort)
import Data.Map (Map, empty, fold, insert, insertWith, member)

type P = (Int, Int)

main = do
  input <- lines <$> readFile "input"
  test <- lines <$> readFile "test_input"
  let problem = input
  let bx = length $ head $ problem
  let by = length $ problem

  print . length . filter (outOfBounds (bx, by)) . map head . group . sort . foldr (\ls acc -> genAntinodes reflect ls ++ acc) [] . parse $ problem
  print . length . map head . group . sort . foldr (\ls acc -> genAntinodes (reflectAll (bx, by)) ls ++ acc) [] . parse $ problem

genAntinodes :: (P -> P -> [P]) -> [P] -> [P]
genAntinodes f [] = []
genAntinodes f [x] = []
genAntinodes f (x : xs) = concatMap (\y -> f x y) xs ++ genAntinodes f xs

reflect :: P -> P -> [P]
reflect (a, b) (c, d) = [(2 * a - c, 2 * b - d), (2 * c - a, 2 * d - b)]

reflectAll :: P -> P -> P -> [P]
reflectAll bounds (a, b) (c, d) = (takeWhile (outOfBounds bounds) $ map (\x -> (a - x * (a - c), b - x * (b - d))) [1 ..]) ++ (takeWhile (outOfBounds bounds) $ map (\x -> (c - x * (c - a), d - x * (d - b))) [1 ..])

outOfBounds :: P -> P -> Bool
outOfBounds (bx, by) (x, y)
  | x < 0 = False
  | y < 0 = False
  | x >= bx = False
  | y >= by = False
  | otherwise = True

parse :: [String] -> Map Char [P]
parse input = parse' 0 0 input
  where
    parse' x y [] = empty
    parse' x y ([] : ls) = parse' 0 (y + 1) ls
    parse' x y ((c : xs) : ls)
      | c == '.' = rest
      | member c rest = insertWith (++) c [(x, y)] rest
      | otherwise = insert c [(x, y)] rest
      where
        rest = parse' (x + 1) y (xs : ls)
