import Data.Char

-- Exercise 1.1
headHunter :: [[a]] -> a
headHunter ((x:_):_) = x
headHunter (_:(x:_):_) = x
headHunter (_:_:(x:_):_) = x
headHunter _ = error "Nope."

-- Exercise 1.2
firstColumn :: [[a]] -> [a]
firstColumn m = [x | (x:_) <- m]

-- Exercise 1.3
shoutOutLoud :: String -> String
shoutOutLoud xs = unwords [replicate 3 x ++ r | (x:r) <- words xs]

-- Exercise 2.1
-- pad :: String -> String -> (String, String)
pad s1 s2 = (s1 ++ (replicate (l - length s1) ' '), s2 ++ (replicate (l - length s2) ' '))
  where l = max (length s1) (length s2)

-- Exercise 3.1
pad' s1 s2 = let
    l = max (length s1) (length s2) 
  in
    (s1 ++ (replicate (l - length s1) ' '), s2 ++ (replicate (l - length s2) ' '))

-- Exercise 4.1
profun p (_:x:_) = "The pair " ++ (case p of
        (1, 1) -> "contains two ones"
        (1, _) -> "contains one one"
        (_, 1) -> "contains one one"
        (_, _) -> "does not containt a single one")
  ++ "and the second element of the list is " ++ show x
