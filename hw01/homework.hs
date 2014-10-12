import Data.Char	

-- 1) function that takes a string and length boundaries and checks whether the string length is within those bounds (inclusive)
strlenInRange str a b
  | a < 0 || b < 0	= error "String length cannot be a negative number"
  | otherwise		= length str >= a && length str <= b

-- 2) function that determines if an element of list with given index is larger than provided value
isHereAGreater list ind val
  | ind < 0 || ind > length list	= False
  | otherwise						= list !! ind > val

-- 3) impure function that gets 2 lines of user input (a sentence and a word) and removes all occurences of the given word in the sentence
wordFilter = do
	sentence <- getLine
	word <- getLine
	let sentence' = rmWord sentence word
	putStrLn sentence'

-- 3) pure function that removes all occurences of a single word in the sentence
rmWord sentence word = unwords [x | x <- words sentence, x /= word]

-- 4) function that returrns a sorted list of given numbers in ascending order
ord3 x y z = if x < y && x < z then [x, min y z, max y z] else if y < z then [y, min x z, max x z] else [z, min x y, max x y]

-- 5a) function that calculates Euclidian norm of a vector
norm (x, y) = sqrt (square x + square y)

-- 5a) function that returns a square of a number
square x = x * x

-- 5b) function that adds two vectors
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- 5c) function that multiplies vector by scalar
scalarMult (x, y) k = (k * x, k * y)

-- 5d) function that caluclates dot product of two vectors
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- 6) function that returns a list of pairs (character, ascii code) for all characters in the given range
asciiRange c1 c2 = zip [c1 .. c2] [ord c1 .. ord c2]

-- 7) function that increments each character of string n times
incn n str = [chr (ord x + n) | x <- str]