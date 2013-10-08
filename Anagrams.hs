-- Anagrams.hs
--
--

import Prelude hiding (subtract)
import Data.List
import Data.Char (toLower)
import Data.Ord (comparing)
import Control.Monad (mapM_)
import qualified Data.Map as Map
import Control.Monad.State

type Occurrences  = [(Char, Int)]

type Dictionary   = [(Occurrences, [String])]

type StateMap a b = State (Map.Map a b) b

printAnagrams :: String -> IO ()
printAnagrams sentence = do
    s <- readFile "resources/linuxwords.txt"
    let d = dictionary $ concatMap words $ lines s
    let prints = putStrLn . concat . intersperse " "
    mapM_ prints $ anagramsM d $ words sentence

anagramsM :: Dictionary -> [String] -> [[String]]
anagramsM d sentence = memoizeM anagramsM' $ occurrences sentence
  where
      anagramsM' :: Monad m => (Occurrences -> m [[String]]) 
                            -> Occurrences -> m [[String]]
      anagramsM' a' []   = return [[]]
      anagramsM' a' occr = return concat `ap` mapM mkAnagramsM combs
        where
            mkAnagramsM soccr
                = return (concatMap (append soccr)) `ap` suffixesM soccr
            append soccr suffix = map (:suffix) $ prefixes soccr
            prefixes soccr = maybe [] id $ wordAnagrams soccr d
            suffixesM soccr = a' $ subtract occr soccr
            combs = filter (/=[]) $ combinations occr

anagrams :: Dictionary -> [String] -> [[String]]
anagrams d sentence = anagrams' $ occurrences sentence
    where          
        anagrams' :: Occurrences -> [[String]]
        anagrams' [] = [[]]
        anagrams' occr
            = concatMap mkAnagrams $ filter (/=[]) $ combinations occr
          where
              mkAnagrams :: Occurrences -> [[String]]
              mkAnagrams soccr 
                  = concatMap (append soccr) $ suffixes soccr
              append soccr suffix  = map (:suffix) $ prefixes soccr
              prefixes soccr = maybe [] id $ wordAnagrams soccr d
              suffixes soccr = anagrams' $ subtract occr soccr

combinations :: Occurrences -> [Occurrences]
combinations []             = [[]]
combinations ((c, f) : cfs)
    = concatMap extendOccs rest ++ rest
  where
      extendOccs  occs = map (:occs) prefixs
      rest        = combinations cfs
      prefixs     = map ((,) c) [1 .. f]

-- yf <= xf if yc == xc, where (yc, yf) in y && (xc, xf) in x
subtract :: Occurrences -> Occurrences -> Occurrences
subtract x []              = x
subtract ((xc, xf) : xcfs) y @ ((yc, yf) : ycfs)
    | xc == yc && xf > yf  = (xc, xf - yf) : subtract xcfs ycfs
    | xc == yc && xf == yf = subtract xcfs ycfs
    | otherwise = (xc, xf) : subtract xcfs y

wordAnagrams :: Occurrences -> Dictionary -> Maybe [String]
wordAnagrams occr d = lookup occr d

occurrences :: [String] -> Occurrences
occurrences = map  mkoccur . group . sortedList
  where
      mkoccur    = combineWith (,) head length
      sortedList = sort . map toLower . concatMap id

dictionary :: [String] -> Dictionary
dictionary
    = map dictEntry . groupBy pairEq . sortBy cmp . map pairs
  where
      dictEntry l = (fst (l !! 0), map snd l)
      pairs     = \s -> (occur s, s)
      cmp       = comparing fst
      occur     = occurrences . (:[])
      pairEq p1 p2 = fst p1 == fst p2

combineWith :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
combineWith f g h x = f (g x) (h x)

memoizeM :: 
    Ord a => ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = evalState (f x) Map.empty 
  where
      g x = do
          y <- t f x  
          m <- get
          put $ Map.insert x y m;
          return y
      f x = get >>= \m -> maybe (g x) return (Map.lookup x m)
