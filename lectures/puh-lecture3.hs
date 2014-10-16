import           Data.Char
import           Data.List

addPairs :: [(Int, Int)] -> [Int]
addPairs xs = [x+y  | (x,y) <- xs]