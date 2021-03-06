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

-- Exercise 5.1
eachTird :: [a] -> [a]
eachTird (_:_:z:rest) = z : eachTird rest
eachTird _ = []

-- Exercise 5.2

-- Exercise 6.1
length' :: [a] -> Int
length' xs = len xs 0
  where len [] n = n
        len (x:xs) n = len xs (n+1)

-- Exercise 6.2
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip ((x,y):ps) = maxx ps (x, y)
  where maxx [] (x, y) = (x, y)
        maxx ((x1, y1) : ps) (x,y) = maxx ps (max x1 x, max y1 y)