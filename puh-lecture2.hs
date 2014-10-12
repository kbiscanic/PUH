import Data.Char
import Data.List

ex21 lst = init $ init $ init $ tail lst

initials s1 s2 = head s1 : (". " ++ (head s2 : "."))

letterCount s = sum [length x | x <- words s, length x > 2]

isPalindrome x = toUppers x == reverse (toUppers x)

toUppers s = [toUpper c | c <- s]

flipp xss = concat $ reverse $ [reverse x | x <- xss]

inCircle r x y = [(a, b) | a <- [-10..10], b <- [-10..10], sq (a-x) + sq (b-y) <= sq r]

sq x = x * x

steps xs = zip xs (tail xs)

index xs = zip [1..] xs

indices x xs = [fst i | i <- index xs, snd i == x]

showLineNumbers s = [show (fst i) ++ ": " ++ snd i | i <- (zip [1..] (lines s))]

haveAlignment xs ys = length [x | x <- index xs, y <- index ys, fst x == fst y, snd x == snd y] > 0