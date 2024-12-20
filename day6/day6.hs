import Data.List (find, group, sort)
import Data.Map (Map, empty, insert, lookup, member)

data Direction = N | E | S | W deriving (Show, Ord, Eq)

directionOffset N = (0, -1)
directionOffset W = (-1, 0)
directionOffset S = (0, 1)
directionOffset E = (1, 0)

guardLocation state = (x, y)
 where
  (x, y, d) = (guardState state)

moveGuard (x, y, N) = (x, y - 1, N)
moveGuard (x, y, W) = (x - 1, y, W)
moveGuard (x, y, S) = (x, y + 1, S)
moveGuard (x, y, E) = (x + 1, y, E)

data State = State
  { guardState :: (Int, Int, Direction)
  , obstacleEdgesMap :: Map (Int, Int, Direction) Direction
  , visitedLocations :: [(Int, Int)]
  , visitedGuardStatesMap :: Map (Int, Int, Direction) Bool
  }
  deriving (Show, Eq)

main = do
  input <- lines <$> readFile "input"
  by <- length <$> pure input
  bx <- length . head <$> pure input
  initialState <- parseInput (State{guardState = (0, 0, N), obstacleEdgesMap = empty, visitedLocations = [], visitedGuardStatesMap = empty}) <$> pure input
  test_state <- simulate bx by <$> pure initialState
  locations <- (map head . group . sort . visitedLocations) <$> pure test_state
  print (length $ group $ sort $ visitedLocations test_state)
  print $ length $ filter (\(x, y) -> simulateEnd bx by $ addObstacle initialState x y) locations

addObstacle :: State -> Int -> Int -> State
addObstacle state x y = state{obstacleEdgesMap = foldl (\m p -> (uncurry insert) p m) (obstacleEdgesMap state) [((x + 1, y, W), N), ((x - 1, y, E), S), ((x, y - 1, S), W), ((x, y + 1, N), E)]}

addGuard :: State -> Int -> Int -> State
addGuard state x y = state{guardState = (x, y, N)}

parseInput :: State -> [String] -> State
parseInput state input = parseInput' 0 0 state input
 where
  parseInput' x y state [] = state
  parseInput' x y state (('.' : xs) : ys) = parseInput' (x + 1) y state (xs : ys)
  parseInput' x y state (('#' : xs) : ys) = parseInput' (x + 1) y (addObstacle state x y) (xs : ys)
  parseInput' x y state (('^' : xs) : ys) = parseInput' (x + 1) y (addGuard state x y) (xs : ys)
  parseInput' x y state ([] : ys) = parseInput' 0 (y + 1) state ys
  parseInput' x y state _ = error "Undefined input"

simulateStep :: State -> State
simulateStep state = case redirect of
  Just d' -> state{guardState = (x, y, d')}
  Nothing -> state{guardState = moveGuard $ guardState state, visitedLocations = (guardLocation state) : (visitedLocations state), visitedGuardStatesMap = insert (guardState state) True (visitedGuardStatesMap state)}
 where
  redirect = Data.Map.lookup (guardState state) (obstacleEdgesMap state)
  (x, y, d) = (guardState state)

simulate :: Int -> Int -> State -> State
simulate bx by state
  | (fst $ guardLocation state) < 0 = state
  | (snd $ guardLocation state) < 0 = state
  | (fst $ guardLocation state) >= bx = state
  | (snd $ guardLocation state) >= by = state
  | otherwise = simulate bx by (simulateStep state)

simulateEnd :: Int -> Int -> State -> Bool
simulateEnd bx by state
  | member (guardState state) (visitedGuardStatesMap state) = True
  | (fst $ guardLocation state) < 0 = False
  | (snd $ guardLocation state) < 0 = False
  | (fst $ guardLocation state) >= bx = False
  | (snd $ guardLocation state) >= by = False
  | otherwise = simulateEnd bx by (simulateStep state)
