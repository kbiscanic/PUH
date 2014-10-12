-- Exercise 1.1:
-- Define 'concat3' that concatenates three strings, but drops the middle one if it's shorter than 2 characters (use 'length' function)
concat3 s1 s2 s3 = s1 ++ (if length s2 < 2 then "" else s2) ++ s3

-- Exercise 1.2:
-- Give a simpler definition of 'showSalary', using only one if-then-else construct
-- Additionally check that salary is non-negative. If it's negative, return an adequate message
showSalary amount bonus = if amount < 0 then error "Salary must be not-negative." else "Salary is " ++ show amount ++ (if bonus /= 0 then ", and a bonus " ++ show bonus else "")