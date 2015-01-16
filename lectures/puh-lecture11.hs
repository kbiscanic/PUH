import Data.Char
import Data.List
import qualified Data.Set as S
import System.Environment
import System.Random

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
  
-- main' :: IO ()
-- main' = putStrLn $ askNumber9
  
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
