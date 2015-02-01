import Data.Maybe
import Data.Bits
import Data.Monoid
import Ratio
import Prelude hiding (Left, Right)
-- 1)
type Position = (Integer, Integer)
data Orientation = Left | Right | Up | Down deriving (Eq, Show)
data TurnDir = CW | CCW deriving (Eq, Show)

-- 1a) 1b) 1c)
data Turtle = Turtle
  { position :: Position 
  , orientation :: Orientation } deriving Show
  
-- 1d)
newTurtle :: Turtle
newTurtle = Turtle (0,0) Up

leonardo = newTurtle

-- 1e)
move :: Integer -> Turtle -> Turtle
move d (Turtle (x,y) o)
  | d < 0 = error "Turtles cannot move backwards"
  | o == Up = Turtle (x,y+d) o
  | o == Down = Turtle (x,y-d) o
  | o == Left = Turtle (x-d,y) o
  | o == Right = Turtle (x+d,y) o
 
-- 1f)
turn :: TurnDir -> Turtle -> Turtle
turn d (Turtle (x,y) o)
  | d == CCW && o == Up = Turtle (x,y) Left
  | d == CCW && o == Down = Turtle (x,y) Right
  | d == CCW && o == Left = Turtle (x,y) Down
  | d == CCW && o == Right = Turtle (x,y) Up
  | d == CW && o == Up = Turtle (x,y) Right
  | d == CW && o == Down = Turtle (x,y) Left
  | d == CW && o == Left = Turtle (x,y) Up
  | d == CW && o == Right = Turtle (x,y) Down
  
-- 1g)
runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle [] t = t
runTurtle (x:xs) t = runTurtle xs $ x t

-- 2)
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

-- 2a)
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ Leaf = Leaf
treeFilter f (Node a l r)
  | f a = Node a (treeFilter f l) (treeFilter f r)
  | otherwise = Leaf

-- 2b)
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap _ Leaf = Leaf
levelMap f n = lmap 0 f n
  where lmap _ _ Leaf = Leaf
        lmap i f (Node a l r) = Node (f i a) (lmap (i+1) f l) (lmap (i+1) f r)
        
-- 2c)
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf _ = True
isSubtree _ Leaf = False
isSubtree n@(Node a l r) (Node a' l' r')
  | a == a' = l == l' && r == r'
  | otherwise = isSubtree n l' || isSubtree n r'
  
-- 3)
data Date = Date
  { day   :: Int
  , month :: Int
  , year  :: Int } deriving (Eq, Show)
  
isLeap :: Int -> Bool
isLeap y = ((y `mod` 4) == 0) && ((y `mod` 100) /= 0) || ((y `mod` 400) == 0)

daysInMonth :: Int -> Int -> Int
daysInMonth m y 
  | m == 2 = if isLeap y then 29 else 28
  | otherwise = 31 - (m-1) `mod` 7 `mod` 2
  
-- 3a)
date :: Int -> Int -> Int -> Maybe Date
date d m y
  | m <= 0 || m > 12 = Nothing
  | d <= 0 || d > (daysInMonth m y) = Nothing
  | otherwise = Just $ Date d m y
  
-- 3b)
addDays :: Date -> Int -> Date
addDays d 0 = d
addDays d n
  | n > 0 = addDays (next d) (n-1)
  | n < 0 = addDays (prev d) (n+1)
  where next d = head $ catMaybes [date (day d + 1) (month d) (year d), date 1 (month d + 1) (year d), date 1 1 (year d + 1)]
        prev d = head $ catMaybes [date (day d - 1) (month d) (year d), date (daysInMonth (month d - 1) (year d)) (month d - 1) (year d), date 31 12 (year d - 1)]
		
-- 4)
data Pred = And Pred Pred | Or Pred Pred | Not Pred | Val Bool deriving Show

expr = And (Or (Val True) (Not (Val True))) (Not (And (Val True) (Val False)))

eval :: Pred -> Bool
eval (Val x) = x
eval (Not x) = not $ eval x
eval (Or x y) = eval x || eval y
eval (And x y) = eval x && eval y

-- 5)
data StackTraceElement = StackTraceElement
  { className  :: String
  , method     :: String
  , lineNumber :: Int }
type StackTrace = [StackTraceElement]

-- 6a)
toGrayCode :: (Integral a, Bits a) => a -> a
toGrayCode x = x `xor` shiftR x 1

-- 6b)
fromGrayCode :: (Integral a, Bits a) => a -> a
fromGrayCode x = unm x (shiftR x 1)
  where unm x 0 = x
        unm x m = unm (x `xor` m) (shiftR m 1)
		
-- 7a)
class Truthy a where
  truey :: a -> Bool
  falsey :: a -> Bool
  
  truey = not . falsey
  falsey = not . truey

instance Truthy Bool where
  truey x = x
  
instance Truthy Int where
  falsey 0 = True
  falsey _ = False
  
instance Truthy [a] where
  falsey [] = True
  falsey _  = False
  
-- 7b)
if' :: Truthy p => p -> a -> a -> a
if' t y n
  | truey t = y
  | otherwise = n
  
-- 7c)
assert :: Truthy p => p -> a -> a
assert a x
  | truey a = x
  | otherwise = error "Assertion failed"
  
-- 7d)
(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
(&&&) x y = truey x && truey y

(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
(|||) x y = truey x || truey y

-- 8a)
data DiffList a = DiffList { undiff :: [a]->[a] }

-- 8b)
empty :: DiffList a
empty = DiffList {
  undiff = ([] ++) }
  
-- 8c)
fromList :: [a] -> DiffList a
fromList xs = DiffList {
  undiff = (xs ++) }
  
-- 8d)
toList :: DiffList a -> [a]
toList dl = undiff dl []

-- 8e)
append :: DiffList a -> DiffList a -> DiffList a
append dl1 dl2 = DiffList {
  undiff = undiff dl1 . undiff dl2 }
  
-- 8f)
instance Monoid (DiffList a) where
  mempty = empty
  mappend = append
  
-- 9) Ratio.hs
(five, three, tenth) = (5 % 1, 15 % 5, 1 % 10)