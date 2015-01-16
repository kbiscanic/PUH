import Data.List
import Data.Char
import Data.Ord

-- 1a)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x = x : takeWhile' f xs
  | otherwise = []
  
-- 1b)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
  | f x = dropWhile' f xs
  | otherwise = xs
  
-- 1c)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f x y | (x,y) <- zip xs ys]

-- 2)
efficientSortBy :: Ord b => (a -> b) -> [a] -> [a]
efficientSortBy f xs = map fst $ sortBy (comparing snd) $ zip xs $ map f xs

-- 3a)
stemmer1 :: String -> String
stemmer1 xs
  | ls >= length xs - ls = s
  | otherwise = xs
  where s = reverse $ tail $ dropWhile (not . isVowel) $ reverse xs
        ls = length s
        isVowel x = elem x "aeiouAEIOU"
       
-- 3b)
suffixes = ["ing", "s", "es", "er"]

stemmer2 :: [String] -> String -> String
stemmer2 ss xs = rec ( reverse (efficientSortBy length (filter (`isSuffixOf` xs) ss)))
  where rec [] = xs
        rec (s:ss) = if length xs - length s >= length s then take (length xs - (length s)) xs else rec ss
        
-- 3c)
stemmer3 :: [String] -> String -> String
stemmer3 ss xs
  | any (`isSuffixOf` xs) ss = stemmer2 ss xs
  | otherwise = stemmer1 xs
  
-- 3d)
pairs = [("driving", "driv"), ("fools", "fool"), ("teacher", "teach")]

testStemmer :: [(String, String)] -> (String -> String) -> Double
testStemmer xs f = (100 * fromIntegral (length ([True | (x, y) <- xs, y == f x]))) / (fromIntegral $ length xs)

-- 3e)
stemText :: (String -> String) -> (String -> Bool) -> String -> String
stemText s p = unwords . map s . filter p . words

-- 4a)
type Point = (Double, Double)
centroid :: [Point] -> Point
centroid [] = error "Cannot calculate centroid of zero points"
centroid ps = ((sum (map fst ps))/lp, (sum (map snd ps))/lp)
  where lp = fromIntegral $ length ps
  
-- 4b)
groupByDist :: [Point] -> [Point] -> [(Point, [Point])]
groupByDist _ [] = error "Cannot group around less than one pont"
groupByDist xs ys = f xs [(y, []) | y <- ys] ys
  where f :: [Point] -> [(Point, [Point])] -> [Point] -> [(Point, [Point])]
        f [] sols _ = sols
        f (x:xs) sols ys = f xs (insert sols (closest x ys) x) ys
        dist :: Point -> Point -> Double
        dist (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2
        closest x ys = snd $ minimum $ zip (map (`dist` x) ys) [0..]
        insert ys i x = take i ys ++ (fst (ys !! i), x : (snd (ys !! i))) : (drop (i+1) ys)
        
-- 4c)
cluster :: [Point] -> Int -> Int -> [(Point, [Point])]
cluster [] _ _ = error "Cannot cluster for no points"
cluster xs k i = if length xs < k
  then error "The number of groups cannot be greater than the number of elements"
  else cls xs i (take k xs)
  where
  cls xs i cs
    | cs' == cs = gxs
    | i == 0 = gxs
    | otherwise = cls xs (i-1) cs'
    where gxs = groupByDist xs cs
          cs' = map (centroid . snd) gxs
          
-- 5a) 
sortTracks :: [String] -> [String]
sortTracks xs = map snd $ sort $ zip (map f xs) xs
  where f :: String -> Int
        f x = read $ head $ dropWhile (not . isNumber . head) $ words x
        
-- 5b)
numberOfPlays :: [String] -> Integer
numberOfPlays xs = sum $ map f xs
  where f x = read $ head $ words x
  
-- 6)
doYouSpeak :: [String] -> String -> Bool
doYouSpeak _ "" = True
doYouSpeak [] _ = False
doYouSpeak xs s = dys (map (map toLower) xs) (map toLower s)
  where dys xs s = or [doYouSpeak xs (drop l s) | l <- ls xs s]
        ls xs s = map length $ filter (`isPrefixOf` s) xs
        
-- 7)
histogram :: String -> IO ()
histogram xs = do
  let xs' = map toLower xs
  let max = maximum $ map (\y -> length $ filter (\x -> y == x) xs') ['a'..'z']
  putStrLn $ unlines $ transpose $ map (\x -> replicate (max - length x + 2) ' ' ++ x) $ map (\y -> replicate (length $ filter (\x -> y == x) xs') '*' ++ '-' : y : []) ['a'..'z']
  
-- 8a)
type Range = Double
smooth :: Range -> (Double -> Double) -> Double -> Double
smooth dx f x = (f (x - dx) + f x + f (x + dx)) / 3

-- 8b)
nfold :: Int -> (a -> a) -> a -> a
nfold n f x = foldl (\x f -> f x) x (replicate n f)

-- 8c)
nsmooth :: Int -> Range -> (Double -> Double) -> Double -> Double
nsmooth n = nfold n . smooth