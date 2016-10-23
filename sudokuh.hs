
-- 
-- An implementation of Peter Norvig's sudoku
-- http://norvig.com/sudoku.html
--
-- instead of maps/dictionaries, I am using Data.Array indexed by
-- tuples
--
-- still work in progress, very initial
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
  then
    Just v
  else
    let e = remove d (v ! i)
        l = length e
    in if l == 0
       then Nothing -- contradiction
       else if l == 1
            then let u = v // [(i, e)]
                     l = zip (peers ! i) $ repeat $ head e
                     w = foldM (\v c -> eliminate v c) u l
                 in w
            else Just $ v // [(i, e)]



parseGrid :: String -> Maybe Values
parseGrid s = foldM (\v c@(i, d) -> assign v c) iniValues $
                    zip (cross rows cols) s

--
-- output functions
--
displayMValues :: Maybe Values -> IO ()
displayMValues (Just v) = (putStrLn . valuesToString) v
displayMValues _ = putStrLn "no solution"

-- valuesToString :: Values -> String
valuesToString v = answer where
  eles = elems v
  s0 = groupsOf 3 $ groupsOf 3 $ groupsOf 3 $ eles
  s1 = (map . map . map . map) (center width) s0
  s2 = (map . map . map) concat               s1
  s3 = (map . map) (intercalate "|")          s2
  s4 = map (intercalate "\n")                 s3
  answer = intercalate hruler                 s4
  r = replicate
  width = 2 + (maximum $ map length eles)
  hruler = "\n" ++ (intercalate "+" $ r 3 $ concat $ r 3 $ r width '-')
                ++ "\n"

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

testGrid1 = "12"
testGrid2 = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
testGrid3 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"

(Just x1) = parseGrid "1"
