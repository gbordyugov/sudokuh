
-- 
-- An implementation of Peter Norvig's sudoku
-- http://norvig.com/sudoku.html
--
-- instead of maps/dictionaries, I am using Data.Array indexed by
-- tuples
--
-- still work in progress, very initial
--

import Data.List
import Data.Array
import Data.Char
import Data.Set (Set, fromList, toList)


data Column = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
              deriving (Eq, Ord, Enum, Ix)

instance Show Column where show = show . (+1) . fromEnum


data Row = RA | RB | RC | RD | RE | RF | RG | RH | RI
           deriving (Eq, Ord, Enum, Ix)

instance Show Row where show x = [chr (ord 'A' + fromEnum x)]


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

gridValues :: String -> (Array Index Char)
gridValues grid' = listArray bnds grid where
  grid = filter (`elem` "0." ++ digits) grid'

assign :: Values -> Index -> Digit -> (Maybe Values)
assign v i d = undefined

eliminate :: Values -> Index -> Digit -> (Maybe Values)
eliminate v i d = undefined

--
-- output functions
--
displayValues :: Values -> IO ()
displayValues = putStrLn . valuesToString

-- valuesToString :: Values -> String
valuesToString v = undefined where
  eles    = elems v
  padded  = map (center width) eles
  triples = groupsOf 3 padded
  width = 1 + (maximum $ map length eles)
valuesToString' v = answer where
  rows        = groupsOf 9 $ elems v
  threes      = map (groupsOf 3) rows
  cell2str l  = (center width) l
  answer      = show threes
  width       = 1 + (maximum $ map length rows)

-- splits a list in groups of n
-- it's faster to reverse the result
-- than keeping conc'ing to growing acc
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

--
-- mimicking Python string methods
--
sjoin :: String -> [String] -> String
sjoin s = concat . intersperse s

testGrid = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"

