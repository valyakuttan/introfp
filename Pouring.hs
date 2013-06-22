-- Pouring.hs
--
--

import Data.List (union, nub)

data Glass = Glass Int Int deriving (Eq) -- Glass amount capacity

instance Show Glass where
    show (Glass a _) = show a

type State = [Glass]

data Move = Empty Int     -- glass index
          | Fill  Int     -- glass index
          | Pour  Int Int -- from, to

instance Show Move where
    show (Empty i)   = "empty " ++  show i
    show (Fill  i)   = "fill " ++ show i
    show (Pour  f t) = "(" ++ show f ++ " -> " ++ show t ++ ")"

data Path = Path {
      history :: [Move]
    , end :: State
    }

instance Show Path where
    show (Path hs e) = show (reverse hs) ++ show e

instance Eq Path where
    Path _ s == Path _ t = s == t

pourings :: Int -> [Int] -> [Path]
pourings goal capacities = filter goalState paths
  where
      goalState          = (goal `elem`) . (map amount) . end
      paths              = from [startPath] [startPath]
      startPath          = Path [] $ map (Glass 0) capacities
      amount (Glass a _) = a

from :: [Path] -> [Path] -> [Path]
from paths explored
    | null paths = []
    | otherwise  = paths ++ from paths' explored'
  where
      paths' = nub . filter unexplored . concatMap nextLevel $ paths
      explored' = union explored paths'
      nextLevel p = map ( extend p) $ possibleMoves . end $ p
      unexplored = (`notElem` explored)

possibleMoves :: State -> [Move]
possibleMoves s = map Empty is ++ map Fill is ++ concatMap pf is
  where
      is   = [0 .. length s - 1]
      pf j = map (Pour j) $ filter (/= j) is

extend :: Path -> Move -> Path
extend (Path hs e) m = Path (m : hs) $ change m e

change :: Move -> State -> State
change m s = case m of
                 Empty i  -> update i s $ empty $ s !! i
                 Fill i   -> update i s $ fill $ s !! i
                 Pour i j -> let (g, h) = pour (s !! i) (s !! j)
                             in update j (update i s g) h

pour :: Glass -> Glass -> (Glass, Glass)
pour g @ (Glass a c1) h @ (Glass b c2) = (g', h')
  where
      g' = transfer (a - w) g
      h' = transfer (b + w) h
      w  = a `min` (c2 - b)

empty :: Glass -> Glass
empty = transfer 0

fill :: Glass -> Glass
fill g @ (Glass _ c) = transfer c g

transfer :: Int -> Glass -> Glass
transfer a (Glass _ c) = Glass a c

update :: Int -> [a] -> a -> [a]
update i xs x
            | 0 <= i && i < length xs = before ++ [x] ++ after
            | otherwise = xs
  where
      before = take i xs
      after  = drop (i + 1) xs
