import Data.Char
import Data.List

-- 1) Define your own version of Data.List.Intercalate, called intercalate’
intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' [] yss = concat yss
intercalate' xs (ys:yss) =  ys ++ xs ++ intercalate' xs yss

-- 2)
type Probability = Double
type DiscreteRandVar = [(Int, Probability)]
x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- 2a) Define an explicitly recursive function mean and an accumulator-style recursive function mean’ that calculate the mean (or expected value) of a discrete random variable
mean :: DiscreteRandVar -> Double
mean [] = 0
mean ((x,p):xs) = fromIntegral x * p + mean xs

mean' :: DiscreteRandVar -> Double
mean' [] = 0
mean' xs = mean'' xs 0
  where mean'' [] acc = acc
        mean'' ((x,p):xs) acc = mean'' xs (fromIntegral x * p + acc)

-- 2b) Define an explicitly recursive function variance and an accumulator-style recursive function variance' that calculate the variance of a discrete random variable
variance :: DiscreteRandVar -> Double
variance [] = 0
variance x = fun x (mean' x)
  where fun [] _ = 0
        fun ((x,p):xs) mi = (fromIntegral x - mi)*(fromIntegral x - mi)*p + fun xs mi
        

variance' :: DiscreteRandVar -> Double
variance' [] = 0
variance' x = fun x (mean' x) 0
  where fun [] _ acc = acc
        fun ((x,p):xs) mi acc = fun xs mi ((fromIntegral x - mi)*(fromIntegral x - mi)*p + acc)

-- 2c) Define an explicitly recursive function probabilityFilter and an accumulatorstyle recursive function probabilityFilter’ that take a probability and a random variable and return a list of values that have at least the given probability of appearing, in the same order in which they appear in the random variable definition.
probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter _ [] = []
probabilityFilter tr ((x,p):xs)
  | p >= tr = x : probabilityFilter tr xs
  | otherwise = probabilityFilter tr xs

probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' _ [] = []
probabilityFilter' tr xs = fun tr xs []
  where fun _ [] acc = acc
        fun tr ((x,p):xs) acc
          | p >= tr = fun tr xs (acc ++ [x])
          | otherwise = fun tr xs acc


-- 3a) Define a function chunk that splits up a list xs into sublist of length n. If the length of xs is not a multiple of n, the last sublist will be shorter than n. 
chunk :: Int -> [a] -> [[a]]
chunk 0 _ = []
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- 3b) Define a function chunkBy that splits up a list xs into sublists of lengths given in a list of indices is. If the lengths in is do not add up to the length of xs, the remaining part of xs will remain unchunked. 
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy [] _ = []
chunkBy _ [] = []
chunkBy (n:ns) xs
  | n > 0 = take n xs : chunkBy ns (drop n xs)
  | otherwise = chunkBy ns xs

-- 3c) Define a function chunkInto that splits up a list xs into n sublists of equal length. If the length of xs is not divisible by n, chunk the remainder into the last sublist. 
chunkInto :: Int -> [a] -> [[a]]
chunkInto 0 _ = []
chunkInto _ [] = []
chunkInto n xs
  | q > 0 = take q xs : chunkInto (n-1) (drop q xs)
  | otherwise = chunkInto (n-1) xs
  where q = quot (length xs) n

-- 4) Define a function rpnCalc that takes a mathematical expression written in Reverse Polish notation and calculates its result. The expression is limited to 1–digit positive integers and operators +,-,*,/, and ^ where / is integer division and ^ is exponentiation
rpnCalc :: String -> Int
rpnCalc [] = 0
rpnCalc xs = calc xs []
  where throw = error "Invalid RPN expression"
        calc [] stack = if length stack /= 1 then throw else head stack
        calc (x:xs) stack
          | x == '+' = if length stack < 2 then throw else calc xs ((x1+x2):stack2)
          | x == '-' = if length stack < 2 then throw else calc xs ((x2-x1):stack2)
          | x == '*' = if length stack < 2 then throw else calc xs ((x1*x2):stack2)
          | x == '/' = if length stack < 2 then throw else calc xs ((div x2 x1):stack2)
          | x == '^' = if length stack < 2 then throw else calc xs ((x2^x1):stack2)
          | isDigit x = calc xs (digitToInt x:stack)
          | otherwise = throw
           where x1 = head stack
                 x2 = head $ tail stack
                 stack2 = tail $ tail stack
                 
-- 5a) Define a function gcd' that calculates the greatest common divisor of two integers, using explicit recursion and the Euclidean algorithm
gcd' :: Int -> Int -> Int
gcd' 0 b = abs b
gcd' a 0 = abs a
gcd' a b = gcd' b $ mod a b

-- 5b) Define a function gcdAll that calculates the greatest common divisor of an arbitrary number of integers given in a list.
gcdAll :: [Int] -> Int
gcdAll = foldr gcd' 0

-- 5c) Define a function extendedGcd which uses the extended Euclidean algorithm to calculate the Bezout coefficients along with the gcd. Given the constants a and b, it calculates x, y and gcd(a,b) that satisfy the expression a*x + b*y = gcd(a,b), returning them in a tuple, in that order.
extendedGcd :: Int -> Int -> (Int, Int, Int)
extendedGcd a 0 = (1, 0, a)
extendedGcd a b = let (x, y, g) = extendedGcd b $ mod a b
  in (y, x - (div a b) * y, g)

-- 6) Implement a function isBipartite that takes an unweighted graph represented as an adjacency list and checks whether the given graph is a bipartite graph
type AdjacencyList = [Int]
type Graph = [AdjacencyList]

isBipartite :: Graph -> Bool
isBipartite [] = True
isBipartite xs = and [not (elem x redl)| x <- blackl]
  where list = map nub $ stuff [1..length xs] xs [1] [] [1]
        redl = list !! 0
        blackl = list !! 1
        stuff [] _ red black _ = [red, black]
        stuff _ _ red black [] = [red, black]
        stuff v al red black (x:q)
          | elem x red = stuff [y|y<-v, y/=x] al red ((al!!(x-1)) ++ black) ([y|y<-al!!(x-1), elem y v] ++ q)
          | elem x black = stuff [y|y<-v, y/=x] al ((al!!(x-1)) ++ red) black ([y|y<-al!!(x-1), elem y v] ++ q)

-- 7) Define a function permutations' that, given a list, returns a list of all its permutations
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (x:xs) = concat [insert ys x | ys <- permutations xs]
  where insert xs x = [insertAt xs n x | n <- [0 .. length xs]]
        insertAt xs n x = concat [[xs !! i | i <- [0 .. n - 1]] ++ x : [xs !! i | i <- [n .. length xs - 1]]]

-- 8) Your job is to define a function frogJumps that, given a number of frogs n, computes the minimal number of jumps necessary for all n frogs to reach the third lily pad
frogJumps :: Int -> Integer
frogJumps 1 = 2
frogJumps n
  | n >= 2 = 3 * frogJumps (n-1) + 2
  | otherwise = error "Not enough frogs"