import Data.Char
import Data.List
import Data.Ord

sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]

filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = intercalate d . map ((:[]) . toUpper . head) . filter p . words

maxDiff :: [Int] -> Int
maxDiff xs@(_:y) = maximum . map (abs . (uncurry (-))) $ zip xs y

studentsPassed :: [(String, Double)] -> [String]
studentsPassed xs = map fst $ filter ((>= pola_m) . snd) xs
  where pola_m = realToFrac(maximum $ map snd xs) / 2
  
isTitleCased :: String -> Bool
isTitleCased xs = and . map (isUpper . head) $ words xs

sortPairs :: Ord b => [(a, b)] -> [(a,b)]
sortPairs = sortBy (comparing snd)

filename :: String -> String
filename xs = reverse $ take 9 $ reverse xs

maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices xs = map snd $ filter (\x -> fst x == max) $ zip xs [0..]
  where max = maximum xs

elem' :: Eq a => a -> [a] -> Bool
elem' y = foldr (\x a -> x == y || a) False

reverse' :: [a] -> [a]
reverse' = foldr (\x a -> a ++ [x]) []

nubRuns :: Eq a => [a] -> [a]
nubRuns xs =
  foldr (\x a@(y:ys) -> if x == y then a else x:a) [last xs] (init xs)
  
reverse'' :: [a] -> [a]
reverse'' = foldl (\a x -> x:a) []

sumEven' :: [Int] -> Int
sumEven' = foldl (\a (x,y) -> if even x then a + y else a) 0 . zip [0..]