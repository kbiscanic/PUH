import           Data.Char
import           Data.List
import           CSVUtils

-- 1) Implement an interleave function that interleaves elements of two lists by sequentially alternating between their elements.
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat [fst pair : [snd pair] | pair <- zip xs ys]

-- 2) Implement a slice function that extracts a slice from a list.
slice :: Int -> Int -> [a] -> [a]
slice i j xs
  | i < 0 || j < 0 || i >= length xs || j >= length xs = error "Slice index out of range"
  | otherwise = drop (min i j) $ take (max i j + 1) xs

-- 3) Define decamel, a function that converts an identifier from lower or upper camel case format to a regular one.
decamel :: String -> String
decamel xs
  | length xs == 0 = error "identifier is empty"
  | all(isAlphaNum) xs =  toLower (head xs) : concat [if isUpper x then ' ' : [toLower x] else [x] | x <- tail xs]
  | otherwise = error "input not in camel case format"

-- 4a} Implement a function count that takes a list of elements belonging to the Eq typeclass and a single element and returns the number of occurrences of the element in the list.
count :: Eq a => [a] -> a -> Int
count xs t = length $ filter(== t) xs 

-- 4b) Implement a function removeUniques that takes a list and returns a list where elements occurring only once have been filtered out.
removeUniques :: Eq a => [a] -> [a]
removeUniques xs = [x | x <- xs, count xs x > 1]

-- 5) Define a function mask that takes a string and a binary mask and preserves a character in the string only when a `1' is present at the same index in the mask.
type Mask = String
mask :: String -> Mask -> String
mask xs [] = mask xs "0"
mask xs m = [fst x | x <- zip xs (cycle m), snd x == '1']

-- 6) Write the function findFriend that takes a list of people and their locations and finds the name of the closest one to a given point.
type Point = (Int, Int)
type Friend = (Point, String)

findFriend :: Point -> [Friend] -> String
findFriend pos ppl
  | length ppl == 0 = error "Nobody exists to be your friend"
  | otherwise = snd $ head $ sort [(dist2 pos (fst p), snd p) | p <- ppl]

-- Function that returns squared distance between 2 points.
dist2 :: Point -> Point -> Int
dist2 p1 p2 = (fst p1 - fst p2)*(fst p1 - fst p2) + (snd p1 - snd p2)*(snd p1 - snd p2)

-- 7a) Define a function mulTable that returns the multiplication table for numbers 1 to n, where n is strictly larger than or equal to 1.
mulTable :: Int -> [[Int]]
mulTable n
  | n < 1 = error "Given number lesser than 1"
  | otherwise = [[i * j | j <- [1..n]] | i <- [1..n]]

-- 7b) Define a function leftpad that converts a given element to a string and leftpads it with spaces up to a given length.
leftpad :: Show a => Int -> a -> String
leftpad n x
  | n < 0 = error "Cannot pad to negative length"
  | length str > n = error $ str ++ " does not fit into " ++ show n ++ " characters"
  | otherwise = take (n - length str) (cycle " ") ++ str
  where str = show x

-- 7c) Define an action named prettyTable that pretty-prints a list of lists (a list of rows of elements) in the form of a correctly-aligned table.
prettyTable :: Show a => [[a]] -> IO ()
prettyTable xss = do
  let width = maximum [length $ show x | xs <- xss, x <- xs] + 1
  let xss' = unlines [concat [leftpad width x | x <- xs] | xs <- xss]
  putStrLn xss'

-- 9a) The wc (wordcount) utility, printing the number of lines, words and characters within a file.
wc :: FilePath -> IO ()
wc path = do
  file <- readFile path
  putStrLn $ show (length (lines file)) ++ " " ++ show (length (words file)) ++ " " ++ show (length file)
  
-- 9b) The paste utility, pairing up lines from two input files and printing them joined with a tab character.
paste :: FilePath -> FilePath -> IO ()
paste path1 path2 = do
  file1 <- readFile path1
  file2 <- readFile path2
  putStrLn $ unlines [fst pair ++ "\t" ++ snd pair | pair <- zip (lines file1) (lines file2)]
  
-- 9c) The cut utility, taking a delimiter, index and a file, then cuts out a portion of each line and writes them out to the standard output.
cut :: String -> Int -> FilePath -> IO ()
cut del n path = if n < 1 then error "Index is to small!" else do
  file <- readFile path
  let fields = [words [if x == del !! 0 then ' ' else x | x <- xs] | xs <- lines file]
  let sol = [xs !! (n-1) | xs <- fields, length xs >= n]
  putStrLn $ show sol

-- Alternative solution to 9c), using code already written before. This solution has some additional restrictions on input data.
cut' :: String -> Int -> FilePath -> IO ()
cut' del n path = do
  csv <- readCSV del path
  let xs = colFields (n-1) csv
  putStrLn $ show xs