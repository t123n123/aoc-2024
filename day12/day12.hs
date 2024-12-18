import Data.List (nub, sort)
import Data.Map (Map, elems, empty, fromList, insert, keys, lookup, toList, updateWithKey)
import Util
import Prelude hiding (lookup)

main = do
  input <- lines <$> readFile "input"
  test <- lines <$> readFile "test_input"
  let problem = input
      symbolMap = makeSymbolMap 0 0 problem
      positions = keys symbolMap
      parentMap :: Map (Int, Int) (Int, Int) = fromList (map (\k -> (k, k)) positions)
      regionMap :: Map (Int, Int) (Int, Int) = fromList (map (\k -> (k, (4, 1))) positions)
      cellPairs =
        concatMap
          ( \(x, y) ->
              map (\(x', y') -> ((x, y), (x', y'))) $
                filter (\(x', y') -> lookup (x', y') symbolMap == lookup (x, y) symbolMap) $
                  map (\(x', y') -> (x + x', y + y')) [(0, 1), (1, 0)]
          )
          positions

      (parentMap', regionMap') = foldl uniteCells (parentMap, regionMap) cellPairs
      getAllParents (mp, ls) node = let (pr, newmp) = getParentUpdate mp node in (newmp, (pr : ls))
      regionRoots = nub $ sort $ snd $ foldl getAllParents (parentMap', []) positions
      regionValues = map (\k -> lookup k regionMap') regionRoots
      corners = countCorners problem parentMap'
      regionScores = map ((\(Just (p, a)) -> p * a)) regionValues

  print $ sum regionScores

check a b c
  | a /= b && a /= c = 1
  | otherwise = 0

cntCorners ((a : b : xs) : (c : d : ys) : zs) = (check a b c, check b a d, check c a d, check d b c)

countCorners :: [String] -> Map (Int, Int) (Int, Int) -> Map (Int, Int) Int
countCorners inp parMap = foldl (\mp (a, b, c, d) -> _) gridwalk inp

uniteCells (parentMap, regionMap) ((x, y), (x', y'))
  | p1 == p2 = (m2, updateWithKey (\k (p, a) -> Just (p - 2, a)) p1 regionMap)
  | otherwise =
      ( reparent m2 p2 p1,
        updateWithKey (\k p -> Just (s1 + s2 - 2, a1 + a2)) p1 regionMap
      )
  where
    (p1, m1) = getParentUpdate parentMap (x, y)
    (p2, m2) = getParentUpdate m1 (x', y')
    Just (s1, a1) = lookup p1 regionMap
    Just (s2, a2) = lookup p2 regionMap

makeSymbolMap :: Int -> Int -> [String] -> Map (Int, Int) Char
makeSymbolMap x y [] = empty
makeSymbolMap x y ([] : ls) = makeSymbolMap 0 (y + 1) ls
makeSymbolMap x y ((c : cs) : ls) = insert (x, y) c $ makeSymbolMap (x + 1) y (cs : ls)

getParentUpdate originalMap node
  | node == p1 = (node, originalMap)
  | p1 == p2 = (p1, originalMap)
  | otherwise = (newParent, reparent newMap node newParent)
  where
    Just p1 = lookup node originalMap
    Just p2 = lookup p1 originalMap
    (newParent, newMap) = getParentUpdate originalMap p1

reparent :: Map (Int, Int) (Int, Int) -> (Int, Int) -> (Int, Int) -> Map (Int, Int) (Int, Int)
reparent originalMap node newParent = updateWithKey (\k p -> Just newParent) node originalMap
