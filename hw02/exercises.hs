import           Data.Char
import           Data.List

-- Exercise 1.1:
-- Define a function that returns a list without the first and last three elements.
cripple xs
  | length xs >= 4      = tail $ init $ init $ init xs
  | otherwise           = error "Not enough elements"

-- Exercise 1.2:
-- Define a function 'initals s1 s2' that takes a person's name and a surname as input and returns a string consisting of person's initials.
initials s1 s2
  | null s1 || null s2  = error "Lists cannot be empty"
  | otherwise           = head s1 : (". " ++ (head s2 : "."))

-- Exercise 1.3:
-- Define a function that concatenates two strings, so that the longest string always comes first.
longCat s1 s2
  | length s1 >= length s2 = s1 ++ s2
  | otherwise              = s2 ++ s1

-- Exercise 1.4:
-- Define a function 'safeHead' that returns an empty list if 'l' is an empty list, otherwise it returns its first element wrapped inside a singleton list.
safeHead l
  | null l    = []
  | otherwise = head l : []

-- Exercise 1.5:
-- Define a function 'hasDuplicates' that checks whether a list contains duplicate elements (use 'nub').
hasDuplicates xs = xs /= nub xs

-- Exercise 2.1:
-- Redefine 'doublesFromTo' so that it also works when b<a.
doublesFromTo a b = [x*2 | x <- [a,(if a > b then pred a else succ a) .. b]]

-- Exercise 2.2:
-- Redefine 'ceasarCode n' so that it shifts all letters a specified number of positions 'n', converts all input to lowercase, and ensures that letters remain within the ['a'..'z'] interval.
ceasarCode n s = [if isLetter c then cycle ['a'..'z'] !! (ord (toLower c) - ord 'a' + n) else c  | c <- s]

-- Exercise 3.1:
-- Define 'letterCount' that computes the total number of letters in a string, hereby ignoring the whitespaces and all words shorter than three letters.
letterCount xs = sum [1 | x <- words xs, length x >= 3, y <- x, isLetter y]

-- Exercise 3.2:
-- Redefine 'isPalindrome' so that it's case insensitive and works correctly for strings that contain whitespaces
isPalindrome s = s1 == reverse s1
  where s1 = concat [[toLower x | x <- xs] | xs <- words s]

-- Exercise 3.3:
-- Define 'flipp xss' that takes a list of lists, reverts each individual list, and concatenates all of them, but in the reverse order.
flipp xss = concat $ reverse [reverse xs | xs <- xss]

-- Exercise 4.1:
-- Define 'inCircle r x y' that returns the coordinates of all points within the ([-10..10],[-10..10]) interval that fall inside a circle of radius 'r' with center '(x,y)'
inCircle r x y = [(a, b) | a <- [-10..10], b <- [-10..10], (a-x)^2 + (b-y)^2 <= r^2]

-- Redefine the function so that it takes the resolution of the grid as an additional argument.
inCircle' r x y res = [(a, b) | a <- [-10,-10 + res .. 10], b <- [-10,-10 + res .. 10], (a-x)^2 + (b-y)^2 <= r^2]

-- Exercise 4.2:
-- Define 'steps xs' that, given a list xs=[x1,x2,..], generates the pairs [(x1,x2),(x2,x3),...]
steps xs = zip xs $ tail xs

-- Exercise 5.1:
-- Define 'indices x xs' that returns the indices of element 'x' in list 'xs' (if 'x' appears multiple times, there will be a number of such indices)
indices x xs = [fst pair | pair <- zip [1..] xs, snd pair == x]

-- Exercise 5.2:
-- Define 'showLineNumbers s' that prefixes all lines from string 's' with a line number.
showLineNumbers s = unlines [show (fst pair) ++ ' ' : snd pair | pair <- (zip [1..] (lines s))]

-- Exercise 5.3:
-- Define 'haveAlignment xs ys' that returns 'True' if 'xs' and 'ys' have any identical elements that are aligned (appear at the same position in both lists)
haveAlignment xs ys = not $ null $ common xs ys

-- Define 'common xs ys' that returns the aligned subsequences.
common xs ys = [fst pair | pair <- zip xs ys, fst pair == snd pair]
