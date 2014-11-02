import Data.List

-- 1) Define a function leftFactorial that calculates the left factorial of a number
leftFactorial :: Integer -> Integer
leftFactorial 0 = 0
leftFactorial x
  | x < 0 = error "Number must be positive!"
  | otherwise = factorial (x - 1) + leftFactorial (x - 1)
  
-- function that calculates the factorial of a number
factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial x
  | x < 0 = error "Number must be positive!"
  | otherwise = x * factorial (x - 1)
  
-- 2) Implement a function factorialZeroes that computes the number of zeroes n! ends with
factorialZeroes :: Int -> Int
factorialZeroes 0 = 0
factorialZeroes x
  | x < 0 = error "Number must be positive!"
  | otherwise = q + factorialZeroes q
  where q = quot x 5
  
-- 3) Define a function interleave that, given a list in format [L1,L2,L3,R1,R2,R3], returns a list formatted as [L1,R1,L2,R2,L3,R3]
interleave :: [a] -> [a]
interleave [] = []
interleave xs
 | odd l = sol ++ [xs !! (l `div` 2)] 
 | otherwise = sol
 where sol = concat [x : [y] | (x, y) <- zip (fst st) (snd st)]
       l = length xs
       st = splitAt ((l + 1) `div` 2) xs
       
-- 4) Define a function pairs that, for a given list containing no duplicates, returns a list of pairs (x,y) such that x differs from y and contains no symmetric pairs (y,x)
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = [(head xs, x) | x <- tail xs] ++ (pairs $ tail xs)

-- 5) Define a function shortestSub that finds the shortest repeating sublist of elements within a given list
-- shortestSub :: Eq a => [a] -> [a]

type Timestamp = [Int]

-- 6a) Define isVaildTimestamp that checks if a timestamp contains valid values.
isValidTimestamp :: Timestamp -> Bool
isValidTimestamp (s:[]) = s >= 0 && s < 60
isValidTimestamp xs@(m:_:[]) = m >= 0 && m < 60 && isValidTimestamp (tail xs)
isValidTimestamp xs@(h:_:_:[]) = h >= 0 && h < 24 && isValidTimestamp (tail xs)
isValidTimestamp _ = False

-- 6b) Define timestampToSec that converts a given timestamp to seconds.
timestampToSec :: Timestamp -> Int
timestampToSec xs
  | isValidTimestamp xs = timestampToSecImpl xs
  | otherwise = error "Invalid timestamp"

timestampToSecImpl :: Timestamp -> Int
timestampToSecImpl (s:[]) = s
timestampToSecImpl (m:s:[]) = m * 60 + s
timestampToSecImpl (h:m:s:[]) = h * 60 * 60 + m * 60 + s
timestampToSecImpl _ = error "Invalid timestamp"

-- 6c) Define timeDiff that calculates a temporal difference, in seconds, between two timestamps
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff ts1 ts2 = abs (timestampToSec ts1 - timestampToSec ts2)

-- 7a) Using group and list comprehensions, define the function counts that, given a list of elements, returns a list of pairs of (element, number of occurrences of the element)
counts :: Ord a => [a] -> [(a, Int)]
counts xs = [(head x, length x)| x <- group $ sort $ xs]

-- 7b) Define a function group’ that does the same as Data.List.group, but doesn’t require the equal elements to be neighbours
group' :: Eq a => [a] -> [[a]]
group' xs = [replicate (count xs x) x | x <- nub xs]

count :: Eq a => [a] -> a -> Int
count xs t = length $ filter(== t) xs 

-- 7c) Using group’ and list comprehensions, define a more general version of the counts function
counts' :: Eq a => [a] -> [(a, Int)]
counts' xs = [(head x, length x)| x <- group' xs]