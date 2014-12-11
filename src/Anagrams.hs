{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

--------------------------------------------------
--
-- |
-- Module: Anagrams
--
--------------------------------------------------

module Anagrams (anagrams) where


import           Control.Monad.State
import           Data.Char            (toLower)
import           Data.List            (group, groupBy, sort, sortBy)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import qualified Data.MemoCombinators as Memo
import           Data.Ord             (comparing)
import           Prelude              hiding (subtract)


type Occurrences  = [(Char, Int)]

type Dictionary   = Map Occurrences [String]

-- | Find anagrams using a dictionary made out
-- of linux words.
anagrams :: String -> IO [String]
anagrams s = do
  ws <- readFile "resources/linuxwords.txt"
  let d = dictionary . concatMap words . lines $ ws
  return $ anagrams' d s

-- | Find all anagrams of a sentence using a dictionary
--
anagrams' :: Dictionary -> String -> [String]
anagrams' d = fmap unwords . memoized . occurrences . words
  where
      memoized = memoize anas
      anas []   = [[]]
      anas occr = concatMap  go combs
        where
            go o = concatMap (append o) $ suffixes o
            append o suffix = map (:suffix) $ prefixes o
            prefixes = fromMaybe [] . (`M.lookup` d)
            suffixes = memoized . subtract occr
            combs    = filter (/=[]) $ combinations occr

      memoize = Memo.list $ Memo.pair Memo.char Memo.integral

-- | Generate all combinations of an Occurrence
--
combinations :: Occurrences -> [Occurrences]
combinations []             = [[]]
combinations ((c, f) : cfs) = concatMap extend rest ++ rest
  where
      extend occs = map ((:occs) . (c,)) [1..f]
      rest        = combinations cfs

-- | subtract a b, subtract the occurence of characters
-- of b from a . It is assumed that for any character
-- c in both a and b occurrence count of c in a >=
-- occurrence count of c in b.
subtract :: Occurrences -> Occurrences -> Occurrences
subtract ((xc, xf) : xcfs) y @ ((yc, yf) : ycfs)
    | xc == yc && xf > yf  = (xc, xf - yf) : subtract xcfs ycfs
    | xc == yc && xf == yf = subtract xcfs ycfs
    | otherwise = (xc, xf) : subtract xcfs y
subtract x _ = x

-- | Compute Occurrences for a list of words
--
occurrences :: [String] -> Occurrences
occurrences = map f . group . sortedList
  where
      f s = (head s, length s)
      sortedList = sort . map toLower . foldr (++) ""

-- | Construct a dictionary which maps Occurrences to words
--
dictionary :: [String] -> Dictionary
dictionary = M.fromList . map dictEntry . ocrToWords
  where
      ocrToWords = groupBy pairEq . sortBy (comparing fst) . map pairs
      dictEntry l = (fst $ head  l, map snd l)
      pairs s = (occurrences [s], s)
      pairEq p1 p2 = fst p1 == fst p2
