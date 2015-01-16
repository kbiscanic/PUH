import Control.Monad

data Sex = Male | Female deriving (Show, Read, Eq, Ord)

data Person = Person {
	personId :: String,
	forename :: String,
	surname :: String,
	sex :: Sex,
	mother :: Maybe Person,
	father :: Maybe Person,
	partner :: Maybe Person,
	children :: [Person]
} deriving (Show, Read, Eq, Ord)

john = Person "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane = Person "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
ann = Person "343" "Ann" "Doe" Female Nothing Nothing Nothing [jane]

partnersMother :: Person -> Maybe Person
partnersMother p = join $ fmap mother $ partner p

-- parentCheck :: Person -> Bool
-- parentCheck p = elem p (maybe [] children (mother p))

data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Ord, Eq)

listHead :: MyList a -> Maybe a
listHead (Cons x _) = Just x
listHead _ = Nothing 

listMap :: (a -> b) -> MyList a -> MyList b
listMap f (Cons x xs) = Cons (f x) $ listMap f xs
listMap _ _ = Empty