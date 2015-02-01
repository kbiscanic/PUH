import Data.Char
import Data.Foldable (Foldable, foldr)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment
import System.Random

data Sex = Male | Female deriving (Show,Read,Eq,Ord)
data Person = Person {
  idNumber :: String,
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  age      :: Int,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Show,Read,Eq,Ord)
  
class Ageing a where
  currentAge :: a -> Int
  maxAge     :: a -> Int
  makeOlder  :: a -> a
  
instance Ageing Person where
  currentAge = age
  makeOlder p = p {age = age p + 1}
  maxAge    _ = 123
  
data Breed = Beagle | Husky | Pekingese deriving (Eq,Ord,Show,Read)
data Dog = Dog {
  dogName  :: String,
  dogBreed :: Breed,
  dogAge   :: Int } deriving (Eq,Ord,Show,Read)
  
instance Ageing Dog where
  currentAge  = dogAge
  makeOlder d = d {dogAge = dogAge d + 1}
  maxAge d    = case dogBreed d of 
                  Husky -> 29
                  _     -> 20

-- 1.1
compareRelativeAge :: (Ageing a, Ageing b) => a -> b -> Ordering
compareRelativeAge x y = compare (fromIntegral (currentAge x) / fromIntegral (maxAge x)) (fromIntegral (currentAge y) / fromIntegral (maxAge y))
				  
-- 1.2
class Nameable a where
  name :: a -> String
  
instance Nameable Dog where
  name d = dogName d ++ " the Dog"
  
instance Nameable Person where
  name p = forename p ++ " " ++ surname p
				  
-- 2.1
class Takeable t where
  takeSome :: Int -> t a -> [a]
				
instance Takeable [] where
  takeSome = take
  
-- 2.2
class Headed t where
  headOf :: t a -> a
  headOff :: t a -> t a
  
instance Headed [] where
  headOf = head
  headOff = tail

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

instance Functor Tree where
  fmap _ Null         = Null
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- 3.1
mapOnTreeMaybe :: (Functor f) => (a -> b) -> f a -> f b
mapOnTreeMaybe = fmap

-- 4.1
sumPositive :: (Foldable t, Num a, Ord a) => t a -> a
sumPositive t = Data.Foldable.foldr (\x c -> if x > 0 then x + c else c) 0 t

-- 4.2
size :: Foldable t => t a -> Int
size t = Data.Foldable.foldr (\x c -> c+1) 0 t

-- 5.1
toSet :: (Foldable t, Ord a) => t a -> S.Set a
toSet = Data.Foldable.foldr S.insert S.empty
  
-- 5.2
indexWords :: String -> M.Map String Int
indexWords s = Data.Foldable.foldr (\x c -> M.insertWith (\a b -> a + b) x 1 c) M.empty (words s)
  
-- 1.1
main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  putStrLn $ reverse $ s1 ++ s2
  threeNumbers
  
-- 1.2
threeNumbers :: IO ()
threeNumbers = do
  x <- getLine
  y <- getLine
  z <- getLine
  putStrLn $ show $ (read x :: Int) + (read y :: Int) + (read z :: Int)
  
-- 2.1
threeStrings :: IO Int
threeStrings = do
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  putStrLn $ s1 ++ s2 ++ s3
  return $ length s1 + length s2 + length s3
  
-- 2.2
askNumber9 :: IO Int
askNumber9 = do
  x <- getLine
  if (not (null x)) && (and $ map isNumber x) then
    return (read x :: Int)
  else askNumber9
  
-- 2.3
askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
  putStrLn m
  x <- askUser'' p :: IO String
  return x
  
askUser'' :: Read a => (String -> Bool) -> IO a
askUser'' p = do
 x <- getLine
 if p x then
   return $ read x
 else askUser'' p
 
askUser' :: (Read a) => String -> (String -> Bool) -> IO a
askUser' m p = do
  putStrLn m
  x <- askUser'' p 
  return x
  
-- 2.4
inputStrings :: IO [String]
inputStrings = do
  s <- liness []
  return s

liness :: [String] -> IO [String]
liness ss = do
  s <- getLine
  if null s then return $ reverse ss
  else liness $ s : ss
  
-- 3.1
fun1 = do
  n' <- getLine
  let n = read n' :: Int
  ss <- linesss n []
  return $ reverse ss
  
linesss :: Int -> [String] -> IO [String]
linesss n ss = if n == 0 then return ss else do
  s <- getLine
  linesss (n-1) (s : ss)
  
-- 3.2
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' ss = seqs ss []

seqs [] xs = return $ reverse xs
seqs (s:ss) xs = do
  x <- s
  seqs ss (x:xs)
  
-- 3.3
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return [] 
mapM' f xs = mapMm f xs []

mapMm _ [] ss = return $ reverse ss
mapMm f (x:xs) ss = do
  s <- f x
  mapMm f xs $ s : ss
  
-- 3.4
pytTri = do
 mapM_ putStrLn $ map show pythagorean

pythagorean = [ (x,y,m*m+n*n) | m <- [2..100], 
                                n <- [1 .. m-1], 
                                let x = m*m-n*n, 
                                let y = 2*m*n,
                                x < 100,
                                y < 100,
                                m*m + n*n < 100 
								]
								
-- 4.1
filterOdd :: IO ()
filterOdd = do
  s <- getContents
  putStr . unlines . map snd . filter (even . fst) $ zip [1..] $ lines s
  
-- 4.2
numberLines :: IO ()
numberLines = interact (unlines . map (\(x, y) ->  show x ++ ' ' : y) . zip [1 ..] . lines)

-- 4.3
filterWords :: S.Set String -> IO ()
filterWords xs = interact (unwords .  filter (\x -> S.notMember x xs) . words)

-- 5.1
wc :: FilePath -> IO (Int, Int, Int)
wc f = do
  s <- readFile f
  return (length s, length $ words s, length $ lines s)
  
-- 5.2
copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines ls f1 f2 = do
  s <- readFile f1
  writeFile f2 $ unlines . map snd . filter (\(l, _) -> elem l ls) . zip [1..] $ lines s

-- 6.1
wordTypes :: FilePath -> IO Int
wordTypes f = do
  s <- readFile f
  return $ length $ nub $ words s  
  
-- 6.2
diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
  s1 <- readFile f1
  s2 <- readFile f2
  let ss = zip (lines s1) (lines s2)
  putStrLn $ unlines . map (\(x, y) -> "<" ++ x ++ "\n" ++ ">" ++ y) $ filter (\(x, y) -> x /= y) ss
  
-- 7.1
fileHead :: IO ()
fileHead = do
  xs <- getArgs
  let n = read $ head xs :: Int
  let f = head $ tail xs
  s <- readFile f
  putStrLn $ unlines $ take n $ lines s
  
-- 8.2
randomPositions :: Int -> Int -> Int -> Int -> IO [(Int, Int)]
randomPositions xl xu yl yu = do
  g <- getStdGen
  g2 <- newStdGen
  return $ zip (randomRs (xl, xu) g) (randomRs(yl, yu) g2)
