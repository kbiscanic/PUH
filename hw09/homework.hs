import Data.List
import Data.Text(pack, unpack)
import qualified Data.Text.IO as IO
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.IO
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment

-- 1) Define a simple utility for measuring the time an action spent executing.
time :: IO () -> IO Double
time fun = do
  start <- getPOSIXTime
  fun
  end <- getPOSIXTime
  let diff = realToFrac (end - start)
  return diff

-- 2) Define a simple grep-like utility.
-- 2a) Read a file and output only the lines containing some given string.
grep :: String -> FilePath -> IO ()
grep s f = do
  xs <- readFile f
  let xs' = unlines $ grepp s $ lines xs
  putStr xs'

grepp :: String -> [String] -> [String]
grepp s = filter (\x -> s `isInfixOf` x)

-- 2b) Use System.Environment.getArgs to read the command-line arguments. Pass
-- them into the previously-defined grep action.
grepWithArgs :: IO ()
grepWithArgs = do
  args <- getArgs
  let s = head args
  let f = head $ tail args
  grep s f

-- 2c) Reimplement grep using the Text type from the Data.Text.Lazy and
-- Data.Text.Lazy.IO modules
grepText :: Data.Text.Lazy.Text -> FilePath -> IO ()
grepText s f = do
  xs <- Data.Text.Lazy.IO.readFile f
  let xs'' = Data.Text.Lazy.unlines $ greppText s $ Data.Text.Lazy.lines xs
  putStr $ Data.Text.Lazy.unpack xs''

greppText :: Data.Text.Lazy.Text -> [Data.Text.Lazy.Text] -> [Data.Text.Lazy.Text]
greppText s = filter (\x -> s `Data.Text.Lazy.isInfixOf` x)

-- 3) Create an API for a simple text-based database (saved in a textual file).
type Table = (FilePath,[String])
-- 3a) Define a function that takes the table name and the column labels (which must
-- be unique) and creates a file named <tableName> ++ ".tbl"
dbCreateTable :: String -> [String] -> IO Table
dbCreateTable table cols = do
  let fName = table ++ ".tbl"
  IO.writeFile fName (pack $ unwords cols ++ "\n")
  return (fName, cols)

-- 3b) Define a function that deletes the file holding the table.
dbDeleteTable :: Table -> IO ()
dbDeleteTable (fName, _) = removeFile fName

-- 3c) Define a function that inserts a row into the table.
dbInsert :: Table -> [String] -> IO ()
dbInsert (fName, cols) row = do
  if length row /= length cols then error "Wrong number of fields!" else return ()
  IO.appendFile fName (pack $ unwords row ++ "\n")

-- 3d) Define a select operation that takes a predicate and returns all rows that
-- satisfy it.
dbSelect :: Table -> ([String] -> Bool) -> IO [[String]]
dbSelect (fName, _) f = do
  xs <- IO.readFile fName
  let xs' = filter f $ map words $ tail $ lines $ unpack xs
  return xs'

-- 3e) Define a delete operation that deletes all rows that satisfy the given
-- predicate.
dbDelete :: Table -> ([String] -> Bool) -> IO ()
dbDelete t@(fName, cols) f = do
  xs <- dbSelect t (not . f)
  dbCreateTable (reverse $ drop 4 $ reverse fName) cols
  mapM_ (\x -> dbInsert t x) xs

-- 3f) Define an update operation that updates all rows that satisfy the given
-- predicate using the given update function.
dbUpdate :: Table -> ([String] -> Bool) -> ([String] -> [String]) -> IO ()
dbUpdate t@(fName, cols) f u = do
  xs <- IO.readFile fName
  let xs' = map(\x -> if f x then u x else x) $ map words $ tail $ lines $ unpack xs
  dbCreateTable (reverse $ drop 4 $ reverse fName) cols
  mapM_ (\x -> dbInsert t x) xs'

-- 3g) Define a print function that prints the table to standard output.
dbPrintTable :: Table -> IO ()
dbPrintTable t@(fName, cols) = do
  xs <- dbSelect t (const True)
  putStrLn $ unwords cols
  putStr $ unlines $ map unwords xs
