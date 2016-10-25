
-- 
-- An implementation of Peter Norvig's sudoku solver
-- http://norvig.com/sudoku.html
--
-- instead of maps/dictionaries, I am using Data.Array indexed by
-- tuples
--
-- still work in progress
--

import Control.Monad

import Data.List
import Data.Array
import Data.Char
import Data.Set (Set, fromList, toList)


data Row = RA | RB | RC | RD | RE | RF | RG | RH | RI
           deriving (Show, Eq, Ord, Enum, Ix)

data Column = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
              deriving (Show, Eq, Ord, Enum, Ix)

type Index = (Row, Column) -- a single cell in the matrix
type Unit = [Index] -- a set of cells

type Digit = Char

digits = ['1'..'9']
cols = [C1 .. ]
rows = [RA .. ]
bnds = ((RA, C1), (RI, C9))

remove e = filter (/=e) -- drops element from a list

cross a b = [(x, y) | x <- a, y <- b]
squares = cross rows cols

unitlist :: [Unit]
unitlist = [cross rows [c]  | c  <- cols] ++
           [cross [r]  cols | r  <- rows] ++
           [cross  rs   cs  | rs <- groupsOf 3 [RA ..],
                              cs <- groupsOf 3 [C1 ..]]

fillArray :: (Index -> a) -> (Array Index a)
fillArray f = listArray bnds [ f s | s <- range bnds]

units :: Array Index [Unit]
units = fillArray $ \s -> [ u | u <- unitlist, elem s u]

peers :: Array Index [Index]
peers = fillArray $ \s -> toList. fromList $ remove s $ concat $ units ! s

-- map from index to set of possible digits

type Values = Array Index [Digit]

iniValues = fillArray $ \s -> digits

assign :: Values -> (Index, Digit) -> Maybe Values
assign v (i, '.') = Just v
assign v (i, '0') = Just v
assign v (i, d) =
  let l = zip (peers ! i) $ repeat d
      u = v // [(i, [d])]
  in foldM (\v c@(i, d) -> eliminate v c) u l


eliminate :: Values -> (Index, Digit) -> Maybe Values
eliminate v (i, d) =
  if not $ elem d $ v ! i
  then Just v
  else let e = remove d $ v ! i
           l = length e
       in let elt = if l == 0
                    then Nothing -- contradiction
                    else if l == 1
                         then assign v (i, head e)
                         else Just $ v // [(i, e)]
          in elt >>= \x -> checkUnits x (i, d)


checkUnits :: Values -> (Index, Digit) -> Maybe Values
checkUnits v (i, d) =
  let us  = units ! i
      jss = [[j | j <- u, d `elem` (v ! j)] | u <- us]
  in foldM f v jss
  where
    f u []     = Nothing
    f u (j:[]) = assign u (j, d)
    f u _      = Just u


search :: Maybe Values -> Maybe Values
search Nothing = Nothing
search (Just v) =
  let lens    = map length [v!i | i <- squares]
      notdone = filter (/=1) lens -- the ambigous ones
  in if length notdone == 0
     then Just v
     else let (n, s) = minimum [(length (v!s), s) | s <- squares
                                                  , length (v!s) > 1]
          in some [search (assign v (s, d)) | d <- v!s]
  where
    some []            = Nothing
    some ((Just x):xs) = Just x
    some (_:xs)        = some xs


groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = reverse $ grp n xs [] where
  grp n [] acc = acc
  grp n xs acc = grp n (drop n xs) ([take n xs] ++ acc)

--
-- mimicking Python string methods
--
center :: Int -> String -> String
center n s = if length s >= n then s else prefix ++ s ++ suffix
  where
    free = n - length s
    before = free `div` 2
    after  = free - before
    prefix = replicate before ' '
    suffix = replicate after  ' '

solve :: String -> Maybe Values
solve = search . parseGrid

displayMValues :: Maybe Values -> IO ()
displayMValues (Just v) = (putStrLn . (++"\n") . valuesToString) v
displayMValues _ = putStrLn "no solution"

solveAndDisply :: String -> IO ()
solveAndDisply = displayMValues . solve

valuesToString :: Values -> String
valuesToString v = answer where
  eles = elems v
  s0 = groupsOf 3 $ groupsOf 3 $ groupsOf 3 $ eles
  s1 = (map . map . map . map) (center width) s0
  s2 = (map . map . map) concat               s1
  s3 = (map . map) (intercalate "|")          s2
  s4 = map (intercalate "\n")                 s3
  answer = intercalate hruler                 s4
  r = replicate
  width = 1 + (maximum $ map length eles)
  hruler = "\n" ++ (intercalate "+" $ r 3 $ concat $ r 3 $ r width '-')
                ++ "\n"

parseGrid :: String -> Maybe Values
parseGrid s =
  foldM (\v c -> assign v c) iniValues $ zip squares s'
  where s'       = filter (`elem` ".0123456789") s

doFile :: String -> IO ()
doFile fname = do
  f <- readFile fname
  mapM_ solveAndDisply $ lines f

main = doFile "top95.txt"

easy   = "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."
hard   = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
