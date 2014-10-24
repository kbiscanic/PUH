module CSVUtils (parseCSV, showCSV, colFields, readCSV, writeCSV) where

import Data.List

type Separator = String
type Document = String
type CSV = [Entry]
type Entry = [Field]
type Field = String
doc = "John;Doe;15\nTom;Sawyer;12\nAnnie;Blake;20"
brokenDoc = "One;Two\nThree;Four;Five"
csv = parseCSV ";" doc

-- 8a) Define a function parseCSV that takes a separator and a string representing a CSV document and returns a CSV representation of the document.
parseCSV :: Separator -> Document -> CSV
parseCSV del doc
  | del !! 0 `notElem` doc = error $ "The character " ++ del ++ " does not occur in the text"
  | any (dim /=) [length x | x <- csv] = error "The CSV file is not well-formed"
  | otherwise = csv
  where
    csv = [words [if x == del !! 0 then ' ' else x | x <- xs] | xs <- lines doc]
    dim = length $ csv !! 00
    
-- 8b) Define a function showCSV that takes a separator and a CSV representation of a document and creates a CSV string from it.
showCSV :: Separator -> CSV -> Document
showCSV del csv
  | length csv == 0 || any (dim /=) [length x | x <- csv] = error "The CSV file is not well-formed"
  | otherwise = unlines [ intercalate del xs | xs <- csv]
  where dim = length $ csv !! 0
  
-- 8c) Define a function colFields that takes a CSV document and a field number and returns a list of fields in that column.
colFields :: Int -> CSV -> [Field]
colFields n csv
  | length csv == 0 || any (dim /=) [length x | x <- csv] = error "The CSV file is not well-formed"
  | n >= dim = error $ "There is no column " ++ show n ++ " in the CSV document"
  | otherwise = [xs !! n | xs <- csv]
  where dim = length $ csv !! 0
  
-- 8d) Define an IO function (an action) readCSV that takes a file path and a separator and returns the CSV representation of the file
readCSV :: Separator -> FilePath -> IO CSV
readCSV del path = do
  file <- readFile path
  return $ parseCSV del file
  
-- 8e) Define a function writeCSV that takes a separator, a file path, and a CSV document and writes the document into a file.
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV del path csv = do
  writeFile path $ showCSV del csv