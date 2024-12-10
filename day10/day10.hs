import Data.List (nub, sort, sortBy, sortOn)
import Data.Map (Map, empty, findWithDefault, insert, keys, mapWithKey)

type P = (Int, Int)

main = do
  input <- (map (concatMap readDigit)) . lines <$> readFile "input"
  test <- (map (concatMap readDigit)) . lines <$> readFile "test_input"
  let problem = input
      bx = length $ head problem
      by = length problem
      posMap = makeMap 0 0 problem empty
      positions = reverse $ sortOn (\k -> findWithDefault (-1) k posMap) (keys posMap)
      res = solve positions posMap
      res' = solve' positions posMap

  print res
  print res'

solve pos vals =
  length $
    concat $
      mapWithKey getZeroPaths $
        foldl (\mp ps -> insert ps (findPaths ps mp vals) mp) empty pos
  where
    getZeroPaths p xs
      | findWithDefault (-1) p vals == 0 = xs
      | otherwise = []

findPaths :: P -> Map P [P] -> Map P Int -> [P]
findPaths (x, y) mp vals = combinePaths . validValue . findNeighbours $ [(-1, 0), (0, -1), (1, 0), (0, 1)]
  where
    pointValue = findWithDefault (-1) (x, y) vals
    findNeighbours = map (\(x', y') -> (x + x', y + y'))
    validValue = filter (\p -> (findWithDefault (-1) p vals) == (pointValue + 1))
    combinePaths = if pointValue == 9 then const [(x, y)] else nub . sort . concat . map (\p -> findWithDefault [] p mp)

solve' pos vals =
  sum $
    mapWithKey getZeroPaths $
      foldl (\mp ps -> insert ps (findPaths' ps mp vals) mp) empty pos
  where
    getZeroPaths p xs
      | findWithDefault (-1) p vals == 0 = xs
      | otherwise = 0

findPaths' :: P -> Map P Int -> Map P Int -> Int
findPaths' (x, y) mp vals = combinePaths . validValue . findNeighbours $ [(-1, 0), (0, -1), (1, 0), (0, 1)]
  where
    pointValue = findWithDefault (-1) (x, y) vals
    findNeighbours = map (\(x', y') -> (x + x', y + y'))
    validValue = filter (\p -> (findWithDefault (-1) p vals) == (pointValue + 1))
    combinePaths = if pointValue == 9 then const 1 else sum . map (\p -> findWithDefault 0 p mp)

makeMap :: Int -> Int -> [[Int]] -> Map (Int, Int) Int -> Map (Int, Int) Int
makeMap x y [] mp = mp
makeMap x y ([] : xs) mp = makeMap 0 (y + 1) xs mp
makeMap x y ((c : xs) : ys) mp = makeMap (x + 1) y (xs : ys) (insert (x, y) c mp)

readDigit :: Char -> [Int]
readDigit '\n' = []
readDigit c = [read [c]]
