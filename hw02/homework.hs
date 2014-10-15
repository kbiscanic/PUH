import           Data.Char
import           Data.List
import           System.Environment

-- 1a) Define a function toTitleCase that takes a String and returns a titlecased version of it.
toTitleCase string = unwords [toUpper (head x) : tail x | x <- words string]

-- 1b) function toTitleCase' that takes an additional argument: a list of words that will remain in lowercase, unless the word occurs at the beginning of the string
toTitleCase' string ignored = unwords [if (((words string) !! 0 == x)) || (notElem x ignored) then toUpper (head x) : tail x else x | x <- words string]

-- 2) Implement a function trimN that takes a list and a number n. It removes n elements from each side of the list. If the trimming would eliminate more elements than the list contains, the list is returned unchanged.
trimN xs n
  | length xs < 2*n     = xs
  | otherwise           = drop n $ take (length xs - n) xs

-- 3) Given a single path as an argument, read the file at the given path and print it out with all of its characters capitalized.
printCapitalizedFile = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ [toUpper x | x <- file]

-- 4) Define a function onlyDivisible that takes a String and a number n. It should drop all characters located at positions that are not divisible by n. Character positions are 0-indexed.
onlyDivisible string n
  | n > 0       = [fst pair | pair <- zip string [0..], snd pair `mod` n == 0]
  | otherwise   = error "n must be positive!"

-- 5) Implement a function triangleCounter that returns the number of distinct triangles with endpoints from the given set. Degenerate triangles (formed using three colinear points) should not be included.
triangleCounter lst = length $ nub [sort [x, y, z] | x <- lst, y <- lst, z <- lst, x /= y, y /= z, z /= x, fst (x) * (snd (y) - snd (z)) + fst (y)*(snd (z) - snd (x)) + fst (z) * (snd (x) - snd (y)) /= 0]

-- 6) Implement a function reverseWords that takes a sentence and reverses the order of words.
reverseWords string = unwords $ reverse $ words string

-- 7) Define intersect' and difference that implement the usual set operators.
intersect' _ [] = []
intersect' [] _ = []
intersect' s1 s2 = [x | x <- s1, x `elem` s2]

difference s [] = s
difference [] _ = []
difference s1 s2 = [x | x <- s1, x `notElem` s2]

-- 8a) The function isWellFormed that checks whether the matrix has all rows of equal length.
isWellFormed mat
  | null mat            = False
  | null (mat !! 0)     = False
  | otherwise           = all (length (mat !! 0)==) [length row | row <- mat]

-- 8b) The function size that returns the dimensions of a n x m matrix as a tuple (n,m).
size mat
  | isWellFormed mat    = (length mat, length (mat !! 0))
  | otherwise           = error "Matrix is malformed"

-- 8c) The function getElement that returns the element at the given position in matrix.
getElement mat row col
  | not (isWellFormed mat)                              = error "Matrix is malformed"
  | row < 0 || col < 0                                  = error "Index out of bounds"
  | row >= fst (size mat) || col >= snd (size mat)      = error "Index out of bounds"
  | otherwise                                           = (mat !! row) !! col

-- 8d) The function getRow that returns the i-th row of a matrix.
getRow mat row
  | not (isWellFormed mat)              = error "Matrix is malformed"
  | row < 0 || row >= fst (size mat)    = error "Index out of bounds"
  | otherwise                           = mat !! row

-- 8e) The function getCol that returns the i-th column of a matrix.
getCol mat col
  | not (isWellFormed mat)              = error "Matrix is malformed"
  | col < 0 || col >= snd (size mat)    = error "Index out of bounds"
  | otherwise                           = [x !! col | x <- mat]

-- 8f) The function addMatrices that returns the sum of two given matrices.
addMatrices mat1 mat2
  | not (isWellFormed mat1) || not (isWellFormed mat2)  = error "Matrix is malformed"
  | size mat1 /= size mat2                              = error "Matrices are not of equal size"
  | otherwise                                           = [[mat1 !! i !! j + mat2 !! i !! j | j <- [0 .. snd (size mat1) - 1]] | i <- [0 .. fst (size mat1) - 1]]

-- 8g) The function transpose' that returns a transposed version of the given matrix.
transpose' mat
  | isWellFormed mat    = [[mat !! i !! j | i <- [0 .. fst (size mat) - 1]] | j <- [0 .. snd (size mat) - 1]]
  | otherwise           = error "Matrix is malformed"

-- 8i) The function multMatrices that multiplies two matrices.
multMatrices mat1 mat2
  | not (isWellFormed mat1) || not (isWellFormed mat2)  = error "Matrix is malformed"
  | snd (size mat1) /= fst (size mat2)                  = error "Incompatible matrix dimensions"
  | otherwise                                           = [[sum[mat1 !! i !! k * mat2 !! k !! j | k <- [0 .. snd (size mat1) - 1]]| j <- [0 .. snd (size mat1) - 1]] | i <- [0 .. fst (size mat1) - 1]]