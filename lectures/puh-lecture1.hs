import           Data.Char
import           Data.List

x = 2

inc x = x + 1

digits2Number x y = x * 10 + y

y = inc 2
z = digits2Number 4 2

name = "Humpty Dumpty"

letter = 'H'

s = "One " ++ "two " ++ "three"

n1 = length "The quick brown fox jumps over the lazy dog"
n2 = length s

condDec x = if x > 0 then x - 1 else x

foo x = (if even x then x*2 else 2) + 1

bigNumber x = if x >= 1000 then True else False
bigNumber' x = x >= 1000

merge s1 s2 = s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2
merge2 s1 s2 = s1 ++ " is " ++ (if s1 < s2 then "not " else "") ++ s2
merge3 s1 s2
  | s1 < s2     = s1 ++ " is " ++ s2
  | otherwise   = s1 ++ " is not " ++ s2

grade score
  | score < 50        = 1
  | score < 63        = 2
  | score < 76        = 3
  | score < 89        = 4
  | otherwise         = 5

showSalary amount bonus
  | bonus /= 0        = "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus
  | otherwise        = "Salary is " ++ show amount

concat3 s1 s2 s3 = s1 ++ (if length s2 < 2 then "" else s2) ++ s3
