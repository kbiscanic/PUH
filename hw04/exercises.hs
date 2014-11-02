import Data.Char
import Data.List

-- Exercise 1.1. Define 'headHunter xss' that takes the head of the first list element. If   the first element has no head, it takes the head of the second element.   If the second element has no head, it takes the head of the third element.   If none of this works, the function returns an error. 
headHunter :: [[a]] -> a
headHunter ((x:_):_) = x
headHunter (_:(x:_):_) = x
headHunter (_:_:(x:_):_) = x
headHunter _ = error "Nope."

-- Exercise 1.2. Define 'firstColumn m' that returns the first column of a matrix.
firstColumn :: [[a]] -> [a]
firstColumn m = [x | (x:_) <- m]

-- Exercise 1.3. Define 'shoutOutLoud' that repeats three times the initial letter of each word in a string.
shoutOutLoud :: String -> String
shoutOutLoud xs = unwords [replicate 3 x ++ r | (x:r) <- words xs]

-- Exercise 2.1. Define 'pad' that pads the shorter of two the strings with trailing spaces and returns both strings capitalized.
pad :: String -> String -> (String, String)
pad (x:xs) (y:ys) = (toUpper x : xs ++ (replicate (l - length xs) ' '), toUpper y : ys ++ (replicate (l - length ys) ' '))
  where l = max (length xs) (length ys)
  
-- Exercise 2.2. Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
quartiles :: [Int] -> (Double,Double,Double)
quartiles xs = (median p1, median ys, median p2)
  where ys = sort xs
        l = length xs
        zs = splitAt (quot l 2) ys
        p1 = fst zs
        p2 = if odd l then tail $ snd zs else snd zs


median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs
        
-- Exercise 3.1 == Exercise 2.1
pad' :: String -> String -> (String, String)
pad' (x:xs) (y:ys) = let l = max (length xs) (length ys) in (toUpper x : xs ++ (replicate (l - length xs) ' '), toUpper y : ys ++ (replicate (l - length ys) ' '))

-- Exercise 3.2 == Exercise 2.2
quartiles' :: [Int] -> (Double,Double,Double)
quartiles' xs = let ys = sort xs
                    l = length xs
                    zs = splitAt (quot l 2) ys
                    p1 = fst zs
                    p2 = if odd l then tail $ snd zs else snd zs
  in (median p1, median ys, median p2)
  
-- Exercise 4.1 Write a function that takes in a pair (a,b) and a list [c] and returns the   following string: "The pair [contains two ones|contains one one|does not contain a single one] and the second element of the list is <x>"
profun :: (Show a, Num t1, Num t, Eq t1, Eq t) => (t, t1) -> [a] -> [Char] 
profun _ [] = error "No second element"
profun _ [_] = error "No Secnod element"
profun p (_:x:_) = "The pair " ++ (case p of
        (1, 1) -> "contains two ones"
        (1, _) -> "contains one one"
        (_, 1) -> "contains one one"
        (_, _) -> "does not contain a single one")
  ++ "and the second element of the list is " ++ show x
