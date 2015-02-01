import Data.Maybe

-- LECTURE 8
-- 1.1
data Date = Date Integer Integer Integer deriving Show

showDate :: Date -> String
showDate (Date d m y) = (show d) ++ "." ++ (show m) ++ "." ++ (show y)

-- 1.2
data Point = Point Double Double deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point  deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x y) (Circle2 (Point x' y') r) = Circle2 (Point (x' + x) (y' + y)) r
translate (Point x y) (Rectangle2 (Point x1 y1) (Point x2 y2)) = Rectangle2 (Point (x1+x) (y1+y)) (Point (x2+x) (y2+y))

-- 1.3
inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point x y) r) (Point x1 y1) = (x1 - x) ^ 2 + (y1 - y) ^ 2 < r ^ 2
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point x y) = x1 < x && x < x2 && y1 < y && y < y2

inShapes :: [Shape2] -> Point -> Bool
inShapes xs p = or $ map (\x -> inShape x p) xs

-- 1.4
data Vehicle = Car String Double | Truck String Double | Motorcycle String Double | Bicycle
totalHorsepower :: [Vehicle] -> Double
totalHorsepower [] = 0
totalHorsepower ((Bicycle):xs) = 0.2 + totalHorsepower xs
totalHorsepower ((Car _ x):xs) = x + totalHorsepower xs
totalHorsepower ((Truck _ x):xs) = x + totalHorsepower xs
totalHorsepower ((Motorcycle _ x):xs) = x + totalHorsepower xs

-- 2.1
data Level = Bachelor | Master | PhD deriving (Show,Eq)
data Student = Student
 { firstName  :: String
 , lastName   :: String
 , studentId  :: String
 , level      :: Level
 , avgGrade   :: Double } deriving Show
 
improveStudent :: Student -> Student
improveStudent s
  | grade <= 4.0 = s {avgGrade = (grade + 1)}
  | otherwise = s
  where grade = avgGrade s
  
-- 2.2
avgGradePerLevels :: [Student] -> (Double,Double,Double)
avgGradePerLevels ss = (bs, ms, phd)
    where
        bs = avg $ filter (\x -> level x == Bachelor) ss
        ms = avg $ filter (\x -> level x == Master) ss
        phd = avg $ filter (\x -> level x == PhD) ss
        avg xs = (foldl (\x (Student _ _ _ _ g) -> x + g) 0 xs) / (foldl (\x _ -> x + 1.0) 0.0 xs)
        
-- 2.4
addStudent :: Student -> [Student] -> [Student]
addStudent x ss
  | elem (studentId x) (map studentId ss) = error "Already exists"
  | otherwise = x:ss
  
-- 3.1
data MyTriplet a b c = MyTriplet
  { fst'  :: a
  , snd'  :: b
  , trd'  :: c } deriving Show
  
toTriplet :: MyTriplet a b c -> (a,b,c)
toTriplet x = (fst' x, snd' x, trd' x)

-- 3.2
data Employee = Employee
  { name   :: String
  , salary :: Maybe Double } deriving Show

totalSalaries :: [Employee] -> Double
totalSalaries xs = sum $ map (\x -> fromMaybe 0 (salary x)) xs

-- 3.3
addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 x ss
  | elem (studentId x) (map studentId ss) = Nothing
  | otherwise = Just $ x:ss
  
-- LECTURE 9
-- 2.1
data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)
listHead :: MyList a -> Maybe a
listHead (Cons x _) = Just x
listHead _ = Nothing

-- 2.2
listMap :: (a -> b) -> MyList a -> MyList b
listMap f (Cons x xs) = Cons (f x) $ listMap f xs
listMap _ _ = Empty

-- 3.1
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)
treeMax :: Ord a => Tree a -> a
treeMax Null = error "empty"
treeMax (Node x _ Null) = x
treeMax (Node _ _ x) = treeMax x

-- 3.2
treeToList :: Ord a => Tree a -> [a]
treeToList Null = []
treeToList (Node x l r) = treeToList l ++ x : treeToList r

-- 4.1
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

listToTree :: Ord a => [a] -> Tree a
listToTree xs = lttr xs Null
  where lttr [] t = t
        lttr (x:xs) t = lttr xs $ treeInsert x t
		
-- 4.2
sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

-- 5.1
data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False
  
-- 5.3
data Sex = Male | Female deriving (Read,Show,Ord,Eq)
data Person = Person {
  idNumber :: String,
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  age      :: Int,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Read,Ord,Eq)
  
instance Show Person where
  show = forename
