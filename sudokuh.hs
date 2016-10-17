
-- 
-- An implementation of Peter Norvig's sudoku
-- http://norvig.com/sudoku.html
--
-- instead of maps/dictionaries, I am using Data.Array indexed by
-- tuples
--
-- still work in progress, very initial
--

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
unitlist = [cross rows [c] | c <- cols] ++
           [cross [r] cols | r <- rows] ++
           [cross rs cs | rs <- [[RA .. RC], [RD .. RF], [RG .. RI]],
                          cs <- [[C1 .. C3], [C4 .. C6], [C7 .. C9]]]


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

display :: Values -> IO ()
display v = 
  let 
    width = 1 + maximum (map length $ elems v)
    -- width = 1 + maximum [1, 2, 3]
    line = ""
  in
    undefined

testGrid = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"

