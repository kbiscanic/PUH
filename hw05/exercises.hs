import Data.Char

-- Exercise 1.1
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- Exercise 1.2
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ((xs:_):xss) = xs : headsOf xss
headsOf ([]:xss) = headsOf xss

-- Exercise 2.1
modMult :: [Int] -> Int -> Int -> [Int]
modMult [] _ _ = []
modMult (x:xs) n m = n `mod` m * x : modMult xs n m

-- Exercise 2.2
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = addPredecessorHelp xs 0

addPredecessorHelp :: Num a => [a] -> a -> [a]
addPredecessorHelp [] _ = []
addPredecessorHelp (x:xs) n = n + x : addPredecessorHelp xs x

-- Exercise 3.1
equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets (ys@(x,y,z):xs)
  | x == y && y == z = ys : equalTriplets xs
  | otherwise = equalTriplets xs

-- Exercise 3.2
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- Exercise 4.1
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' n xs
  | n < 0 = drop' (-n) (reverse xs)
  | otherwise = drop' n xs

-- Exercise 4.2
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _ [] = []
takeFromTo n1 n2 (x:xs)
  | n1 > 0 = takeFromTo (n1-1) (n2-1) xs
  | n2 >= 0 = x : takeFromTo (n1-1) (n2-1) xs
  | otherwise = []

-- Exercise 5.1
eachTird :: [a] -> [a]
eachTird (_:_:z:rest) = z : eachTird rest
eachTird _ = []

-- Exercise 5.2
crossZip :: [a] -> [b] -> [(a,b)]
crossZip [] _ = []
crossZip _ [] = []
crossZip (_:[]) _ = []
crossZip _ (_:[]) = []
crossZip (x1:x2:xs) (y1:y2:ys) = (x1,y2) : (x2,y1) : crossZip xs ys

-- Exercise 6.1
length' :: [a] -> Int
length' xs = len xs 0
  where len [] n = n
        len (x:xs) n = len xs (n+1)

-- Exercise 6.2
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip ((x,y):ps) = maxx ps (x, y)
  where maxx [] (x, y) = (x, y)
        maxx ((x1, y1) : ps) (x,y) = maxx ps (max x1 x, max y1 y)

maxUnzip' :: [(Int,Int)] -> (Int,Int)
maxUnzip' [] = error "empty list"
maxUnzip' ((x,y) : ps) = (max x x', max y y')
  where rec = maxx ps
        x' = fst rec
        y' = snd rec
        maxx ((x,y):[]) = (x,y)