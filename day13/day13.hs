import Data.Char (isDigit)

type P = (Int, Int)

gtP (a, b) (c, d) = (a > c || b > d)

addP (a, b) (c, d) = (a + c, b + d)

multP (a, b) c = (a * c, b * c)

main = do
  input <- lines <$> readFile "input"
  test <- lines <$> readFile "test_input"
  let problem = test
      problems = splitOnElem "" problem
      problemValues = map parseProblem problems
      results = map solveProblem problemValues

  print results

splitOnElem :: (Eq a) => a -> [a] -> [[a]]
splitOnElem x [] = []
splitOnElem x xs = (takeWhile (/= x) xs) : (splitOnElem x (drop 1 $ dropWhile (/= x) xs))

parseProblem :: [String] -> ((P, P), P)
parseProblem [a, b, x] = (((ax, ay), (bx, by)), (xx, xy))
  where
    [ax, ay] = map (read :: String -> Int) $ map (filter isDigit) $ splitOnElem ',' a
    [bx, by] = map (read :: String -> Int) $ map (filter isDigit) $ splitOnElem ',' b
    [xx, xy] = map (read :: String -> Int) $ map (filter isDigit) $ splitOnElem ',' x
parseProblem _ = error "unknown problem input"

solveProblem :: ((P, P), P) -> Int
solveProblem ((a, b), x) = solveProblem' (0, 0) x
  where
    solveProblem' s x
      | gtP s x = 1000000000
      | s == x = 0
      | otherwise = min (solveProblem' (addP s a) x) (solveProblem' (addP s a) x)
