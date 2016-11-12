
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


{-
--
-- custom types seem to be really slow
--
data Row = RA | RB | RC | RD | RE | RF | RG | RH | RI
           deriving (Show, Eq, Ord, Enum, Ix)

data Column = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
              deriving (Show, Eq, Ord, Enum, Ix)

rows = [RA .. ]
cols = [C1 .. ]
bnds = ((RA, C1), (RI, C9))
-}

type Row    = Char
type Column = Char
rows = ['A'..'I']
cols = ['1'..'9']
bnds = (('A', '1'), ('I', '9'))

type Index = (Row, Column) -- a single cell in the matrix
type Unit = [Index] -- a set of cells

type Digit = Char

digits = ['1'..'9']

remove e = filter (/=e) -- drops element from a list

cross a b = [(x, y) | x <- a, y <- b]
squares = cross rows cols

unitlist :: [Unit]
unitlist = [cross rows [c]  | c  <- cols] ++
           [cross [r]  cols | r  <- rows] ++
           [cross  rs   cs  | rs <- groupsOf 3 rows,
                              cs <- groupsOf 3 cols]

fillArray :: (Index -> a) -> Array Index a
fillArray f = listArray bnds [ f s | s <- range bnds]

units :: Array Index [Unit]
units = fillArray $ \s -> [ u | u <- unitlist, s `elem` u]

peers :: Array Index [Index]
peers = fillArray $ \s -> toList. fromList $ remove s $ concat $ units ! s



-- map from index to set of possible digits

type Values = Array Index [Digit]


iniValues = fillArray $ const digits

--
-- if f x returns Nothing, continue with x
--
noncrit :: (a -> Maybe a) -> a -> Maybe a
noncrit f x = case f x of
  Nothing -> Just x
  Just y  -> Just y

--
-- assign a value to a cell by sequentially eliminating all other values
--
assign :: Values -> (Index, Digit) -> Maybe Values
assign v (_, '.') = Just v
assign v (_, '0') = Just v
assign v (i, d) =
  let o = delete d $ v ! i
      l = zip (repeat i) o
  in foldM eliminate v l


--
-- eliminates value d from cell i
--
eliminate :: Values -> (Index, Digit) -> Maybe Values
eliminate v (i, d) =
  let ds = v ! i                -- possible digits before elimination
  in if d `notElem` ds          -- is already eliminated?
     then Just v                -- do nothing
     else let e = delete d ds   -- remaining digits
              u = v // [(i, e)]
          in let w = case e of
                  []  -> Nothing               -- contradiction
                  [c] -> foldM eliminate u (zip (peers ! i) (repeat c))
                  _   -> Just u                -- nothing special
             -- in w >>= \x -> checkUnits x (i, d)
             in w >>= \x -> foldM (locate d) x (units ! i)

--
-- Manu / Daniel Fischer's version of this func
--
-- why is it faster?
--
locate :: Digit -> Values -> Unit -> Maybe Values
locate d v u = case filter ((d `elem`) . (v!)) u of
  []  -> Nothing
  [s] -> assign v (s, d)
  _   -> return v


--
-- digit d has just been eliminated from cell i
-- it can happen that in some unit of i, digit d can now appear
-- just on one position
-- if this is the case, assign d to that position
--
checkUnits :: Values -> (Index, Digit) -> Maybe Values
checkUnits v (i, d) =
  let jss = [ [j | j <- u, d `elem` (v ! j)] | u <- units ! i]
  in foldM f v jss
  where
    f u []  = Nothing         -- contradiction, no place for this value
    f u [j] = assign u (j, d) -- d can only be here, assign it
    f u _   = Just u          -- d appears more than once in this unit


search :: Maybe Values -> Maybe Values
search Nothing = Nothing
search (Just v) =
  let lens    = map length [v!i | i <- squares]
      notdone = filter (/=1) lens -- the ambigous ones
  in if null notdone
     then Just v
     else let (n, s) = minimum [(length (v!s), s) | s <- squares
                                                  , length (v!s) > 1]
          in some [search (assign v (s, d)) | d <- v!s]
  where
    some []            = Nothing
    some (Just x : xs) = Just x
    some (_:xs)        = some xs

--
--
-- Search by Manu & Daniel Fischer
--
search' :: Maybe Values -> Maybe Values
search' Nothing  = Nothing
search' (Just g) =
  case [(l,(s,xs)) | (s,xs) <- assocs g, let l = length xs, l /= 1] of
            [] -> return g
            ls -> do let (_,(s,ds)) = minimum ls
                     -- msum [assign g (s,d) >>= search' | d <- ds]
                     msum [search' (assign g (s,d)) | d <- ds]

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

solveAndDisplay :: String -> IO ()
solveAndDisplay = displayMValues . solve

valuesToString :: Values -> String
valuesToString v = s4 where
  eles = elems v
  s0 = map (center width)    eles
  s1 = mg (intercalate "")   s0
  s2 = mg (intercalate "|")  s1
  s3 = mg (intercalate "\n") s2
  s4 = intercalate hruler    s3
  r = replicate
  width = 1 + maximum (map length eles)
  hruler = "\n" ++ intercalate "+" (r 3 $ concat $ r 3 $ r width '-')
                ++ "\n"
  mg f xs = map f $ groupsOf 3 xs

parseGrid :: String -> Maybe Values
parseGrid s =
  foldM assign iniValues $ zip squares s'
  where s'       = filter (`elem` ".0123456789") s

doFile :: String -> IO ()
doFile fname = do
  f <- readFile fname
  mapM_ solveAndDisplay $ lines f

main = doFile "top95.txt"

easy = "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."
hard = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
