module Ratio (Ratio, (%)) where

-- 9a)
data Ratio = Ratio
  { p :: Integer
  , q :: Integer }

-- 9b)
(%) :: (Integral a, Integral b) => a -> b -> Ratio
(%) _ 0 = error "Division by zero"
(%) x y
  | y > 0     = Ratio (fromIntegral x) (fromIntegral y)
  | otherwise = Ratio (fromIntegral (-x)) (fromIntegral (-y))

-- 9c)
instance Eq Ratio where
  x == y = fromIntegral (p x) / fromIntegral (q x) == fromIntegral (p y) / fromIntegral (q y)
  
-- 9d)
instance Ord Ratio where
  (<=) x y = fromIntegral (p x) / fromIntegral (q x) <= fromIntegral (p y) / fromIntegral (q y)
  
-- 9e)
instance Num Ratio where
  (*) x y       = Ratio (p x * p y) (q x * q y)
  (+) x y       = Ratio (p x * q y + p y * q x) (q x * q y)
  (-) x y       = (+) x (negate y)
  negate x      = Ratio (- p x) (q x)
  abs x         = Ratio (abs (p x)) (q x) 
  signum x      | x < 0     = negate 1
                | x == 0    = 0
                | otherwise = 1
  fromInteger x = (%) x 1
  
-- 9f)
instance Show Ratio where
  show x = show p' ++ " % " ++ show q'
    where p' = p x `div` gc
          q' = q x `div` gc
          gc = gcd (p x) (q x)