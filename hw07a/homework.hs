import Data.Char
import Data.List
import qualified Data.Vector as V

-- 1) Define a function partition to partition a list using a user-provided predicate function
partition :: [a -> Bool] -> [a] -> [[a]]
partition ps xs = [filter p xs | p <- ps]

-- 2) Define a function cycleMap fs xs that maps various functions from fs over a list xs, depending on the index of an element in the list
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] _ = []
cycleMap fs xs = [f x | (x,f) <- zip xs (cycle fs)]

-- 3a) Define an explicitly recursive function reduce that reduces a list of elements to a single element using a seed value and a binary reduction function
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ seed [] = seed
reduce f seed (x:xs) = reduce f (f seed x) xs

-- 3b) Define a variant of reduce called reduce1 that behaves like reduce, but assumes the input list contains at least one element and so eschews taking a seed element.
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ [] = error "reduce1 got an empty list"
reduce1 f (x:xs) = reduce f x xs

-- 3c) Define a function scan that performs similarly to reduce, but returns a list of all the intermediate values with the result at the end instead of just the last result.
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ seed [] = [seed]
scan f seed (x:xs) = seed : scan f (f seed x) xs

-- 4a) Define a variant of reduce that performs similarly, only does the operations from right to left, instead.
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce _ seed [] = seed
rreduce f seed xs = rreduce f (f (last xs) seed) (init xs)

-- 4b) Define a variant of rreduce called rreduce1 that behaves like rreduce, but assumes the input list contains at least one element
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ [] = error "rreduce1 got an empty list"
rreduce1 f xs = rreduce f (last xs) (init xs)

-- 4c) Define a variant of the scan function that works from right to left, called rscan.
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan _ seed [] = [seed]
rscan f seed xs = rscan f (f (last xs) seed) (init xs) ++ [seed]

-- 5a) Define newton, a function that computes an approximation of the square root of a number using a special case of Newton’s method.
type Tolerance = Double
newton :: Tolerance -> Double -> Double
newton tol x
  | x < 0 = error "can't get sqrt of negative number"
  | otherwise = new tol x 1
    where new tol x y = if abs (y' - y) < tol then y' else new tol x y'
            where y' = (y+x/y)/2

-- 5b) Define deriv, a function that computes the derivative of a given function
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x+dx) - f x) / dx
  where dx = 0.00001
  
-- 6) Define a function isHappy that checks whether a number is a happy number. 
-- isHappy :: Int -> Bool
isHappy num = happy num []
  where happy x seen
          | x `elem` seen = False
          | otherwise = x' == 1 || happy x' (x : seen)
          where x' = sum $ map (^2) $ digits x
        digits 0 = []
        digits x = digits (x `div` 10) ++ [x `mod` 10]

-- 7a) Define a function split which recursively splits a list into two sublists at the middle
split :: [a] -> ([a], [a])
split xs = spl xs (l `div` 2) ([],[])
  where spl [] _ (xs1, xs2) = (reverse xs1, reverse xs2)
        spl (x:xs) n (xs1, xs2) = if n > 0 then spl xs (n-1) (x:xs1, xs2) else spl xs n (xs1, x:xs2)
        l = length xs + 1

-- 7b) Using the split function you just wrote, implement mergesort, a function which performs the mergesort sorting algorithm on a list.
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge f xs [] = xs
merge f [] ys = ys
merge f (x:xs) (y:ys)
    | f x y = x: merge f xs (y:ys)
    | otherwise = y: merge f (x:xs) ys

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (<=) (mergesort xs1) (mergesort xs2)
  where (xs1, xs2) = split xs
  
-- 7c) Define the function countInversions which counts how many swaps of adjecent elements have to be performed to sort a list.
merge' :: (a -> a -> Bool) -> [a] -> [a] -> Int -> Int
merge' f xs [] acc = acc
merge' f [] ys acc = acc
merge' f (x:xs) (y:ys) acc
    | f x y = merge' f xs (y:ys) acc
    | otherwise = merge' f (x:xs) ys (acc + length xs + 1)

countInversions :: Ord a => [a] -> Int
countInversions [] = 0
countInversions [x] = 0
countInversions xs = countInversions xs1 + countInversions xs2 + merge' (<=) (mergesort xs1) (mergesort xs2) 0
  where (xs1, xs2) = split xs

-- 7d) Define a second version of mergesort, called mergesort’. It should operate on Data.Vector instead.
mergeV :: (a -> a -> Bool) -> V.Vector a -> V.Vector a -> V.Vector a
mergeV f xs ys
  | V.null xs = ys
  | V.null ys = xs
  | f x y = V.cons x (mergeV f (V.tail xs) ys)
  | otherwise = V.cons y (mergeV f xs (V.tail ys))
  where x = V.head xs
        y = V.head ys

mergesort' :: Ord a => V.Vector a -> V.Vector a
mergesort' v
  | V.null v = V.empty
  | V.length v == 1 = v
  | otherwise = mergeV (<=) (mergesort' xs1) (mergesort' xs2)
  where (xs1, xs2) = V.splitAt (div (V.length v + 1) 2) v

-- 8) defining a function pacMan that takes a number n and a list of dependencies and returns a list of n elements describing the order in which packages should be installed.
type Package = Int
type Dependency = (Package,Package)
pacMan :: Int -> [Dependency] -> [Package]
pacMan n deps = pac [] (s deps []) deps
  where s ss ls = [x | x <- [1..n], not(or [y == x | (y,_) <- ss]), x `notElem` ls]
        pac ls [] [] = reverse ls
        pac _ [] _ = error "Impossible to resolve"
        pac ls (m:ss) es = pac ls' (s es' ls') es'
          where ls' = m:ls
                es' = rm es m
        rm es m = [e | e@(x,y) <- es, y /= m]