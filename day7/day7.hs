main = do
    input <- map (mapBoth readInt (map readInt . words) . mySplit ':') . lines <$> readFile "input"
    print . foldl max 0 . map (\x -> length $ snd x) $ input
    print . sum . map (\x -> fst x) . filter (\x -> check (fst x) (snd x)) $ input
    print . sum . map (\x -> fst x) . filter (\x -> check2 (fst x) (snd x)) $ input

check :: Int -> [Int] -> Bool
check t [] = False
check t (x : xs) = check' t [x] xs
  where
    check' t ls [] = elem t ls
    check' t ls (x : xs) = check' t (filter (\x -> x <= t) ((map (+ x) ls) ++ (map (* x) ls))) xs

concat' :: Int -> Int -> Int
concat' x y = readInt $ show x ++ show y

check2 :: Int -> [Int] -> Bool
check2 t [] = False
check2 t (x : xs) = check2' t [x] xs
  where
    check2' t ls [] = elem t ls
    check2' t ls (x : xs) = check2' t (filter (\x -> x <= t) $ (map (+ x) ls) ++ (map (* x) ls) ++ (map (\y -> concat' y x) ls)) xs

mySplit x xs = splitAt' x xs []
  where
    splitAt' x [] ys = (reverse ys, [])
    splitAt' x (x' : xs) ys
        | (x == x') = (reverse ys, xs)
        | otherwise = splitAt' x xs (x' : ys)

mapBoth f g (a, b) = (f a, g b)

readInt = read :: String -> Int
