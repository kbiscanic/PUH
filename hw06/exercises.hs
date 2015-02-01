import Data.Char

-- Exercise 1.1
takeThree = take 3
dropThree = drop 3
hundredTimes = replicate 100

-- Exercise 1.2
index = zip [0..]
index' = (`zip` [0..])

-- Exercise 1.3
divider = (`replicate` '=')

-- Exercise 2.1
applyOnLast f xs ys = f (last xs) (last ys)
lastTwoPlus100 = applyOnLast (addThree 100)

addThree x y z = x + y + z

-- Exercise 2.2
applyManyTimes n f x
  | n <= 0 = x
  | otherwise = applyManyTimes (n-1) f (f x)

applyTwice = applyManyTimes 2

-- Exercise 3.1
listifylist = map (:[])

-- Exercise 3.2
cutoff n = map (min n)

-- Exercise 4.1
sumEvenSquares xs = sum $ map (^2) $ filter even xs

-- Exercise 4.2
freq x xs = length $ filter (x==) xs

-- Exercise 4.3
freqFilter n xs =  map fst $ filter (\x -> snd x >= n) $ map (\x -> (x, freq x xs)) xs

-- Exercise 5.1
withinInterval n m = filter (\x -> x >= n && x <= m)

-- Exercise 5.2
sndColumn = map (\x -> x!!1)

-- Exercise 5.3
canonicalizePairs xs = filter (\(x,y) -> x /= y) xs