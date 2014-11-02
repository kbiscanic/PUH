import Data.List
import Data.Char

-- 1) Define a function leftFactorial that calculates the left factorial of a number
leftFactorial :: Integer -> Integer
leftFactorial 0 = 0
leftFactorial x
  | x < 0 = error "Number must be positive!"
  | otherwise = factorial (x - 1) + leftFactorial (x - 1)
  where factorial 0 = 1
        factorial 1 = 1
        factorial x = x * factorial (x - 1)
  
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
shortestSub [] = []
shortestSub xs = ssub xs (tail $ inits xs)
  where ssub xs (y:ys) = if take (length xs) (cycle y) == xs then y else ssub xs ys

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
  | isValidTimestamp xs = ttsi xs
  | otherwise = error "Invalid timestamp"
  where ttsi (s:[]) = s
        ttsi (m:s:[]) = m * 60 + s
        ttsi (h:m:s:[]) = h * 60 * 60 + m * 60 + s

-- 6c) Define timeDiff that calculates a temporal difference, in seconds, between two timestamps
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff ts1 ts2 = abs (timestampToSec ts1 - timestampToSec ts2)

-- 7a) Using group and list comprehensions, define the function counts that, given a list of elements, returns a list of pairs of (element, number of occurrences of the element)
counts :: Ord a => [a] -> [(a, Int)]
counts xs = [(head x, length x)| x <- group $ sort $ xs]

-- 7b) Define a function group’ that does the same as Data.List.group, but doesn’t require the equal elements to be neighbours
group' :: Eq a => [a] -> [[a]]
group' xs = [replicate (count xs x) x | x <- nub xs]
  where count xs t = length $ filter(== t) xs 

-- 7c) Using group’ and list comprehensions, define a more general version of the counts function
counts' :: Eq a => [a] -> [(a, Int)]
counts' xs = [(head x, length x)| x <- group' xs]


type Grid = [String]

checkGrid :: Grid -> Bool
checkGrid [] = False
checkGrid xss = and [length xs == l | xs <- xss]
  where l = length (xss !! 0)

-- 8a) Define a function lightsOutLite that takes a Grid and computes the minimal number of moves to complete a simplified version of Lights Out!.
lightsOutLite :: Grid -> Int
lightsOutLite xss
  | checkGrid xss = sum [digitToInt x | x <- concat xss]
  | otherwise = error "Broken grid!"
  
-- 9a) Implement a function oneEdits that, given a String, returns all possible strings that are one edit{distance away from it.

extraChar :: String -> [String]
extraChar xs = concat [insert xs n | n <- [-1 .. length xs - 1]]
  where insert xs n = [[xs !! i | i <- [0 .. n]] ++ y : [xs !! i | i <- [n + 1 .. length xs - 1]] | y <- ['a' .. 'z']]
  
lessChar :: String -> [String]
lessChar xs = [[xs !! i | i <- [0 .. length xs - 1], i /= j] | j <- [0 .. length xs - 1]]

mutatedChar :: String -> [String]
mutatedChar xs = concat [mutate xs n | n <- [0 .. length xs -1]]
  where mutate xs n = [[xs !! i | i <- [0 .. n - 1]] ++ y : [xs !! i | i <- [n + 1 .. length xs - 1]] | y <- ['a' .. 'z']]
  
swappedChars :: String -> [String]
swappedChars xs = [swap xs n (n+1) | n <- [0 .. length xs - 2]]
  where swap xs i j = concat [[if n == i then xs !! j else if n == j then xs !! i else xs !! n] | n <- [0 .. length xs - 1]]
  
oneEdits :: String -> [String]
oneEdits xs = sort $ nub $ extraChar xs ++ lessChar xs ++ mutatedChar xs ++ swappedChars xs

-- 9b) Now define a function twoEdits, that does the same as oneEdits, but edit distances of 2 instead
twoEdits :: String -> [String]
twoEdits xs = sort $ nub $ concat [oneEdits x | x <- oneEdits xs]

compareToFile :: (String -> [String]) -> String -> FilePath -> IO Bool
compareToFile f s file = do
  list <- readFile file
  return $ f s == (read list :: [String])
  
testOneEdits :: IO Bool
testOneEdits = compareToFile oneEdits "hi" "oneEdits.txt"

testTwoEdits :: IO Bool
testTwoEdits = compareToFile twoEdits "hi" "twoEdits.txt"

-- 10a) Define fromSeed that generates a pseudorandom integer given a seed value.
type Seed = Int
m = 2 ^ 32
a = 1664525
c = 1013904223

fromSeed :: Seed -> Int
fromSeed x = (a * x + c) `mod` m

-- 10b) Define guess, a game that, given a starting seed value and number limit, asks the player to guess a number between 0 and the limit (inclusively).
guess :: Seed -> Int -> IO Ordering
guess x l = do
  let num = fromSeed x `mod` l
  putStr "guess: "
  input <- readLn
  return $ compare num input