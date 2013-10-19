-- Anagrams.hs
--
--
{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

import Prelude hiding (subtract)
import Data.List
import Data.Char (toLower)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Applicative ((<$>))

type Occurrences  = [(Char, Int)]

type Dictionary   = Map Occurrences [String]

data Memo = M !(Map Occurrences [[String]])

type MemoT = State Memo

newtype MemoM a = MemoM {
    runMemoT :: MemoT a 
   } deriving (Functor, Monad, MonadState Memo)

anagrams' :: String -> IO [String]
anagrams' s = do
  xs <- readFile "resources/linuxwords.txt"
  let dict = dictionary . concatMap words . lines $! xs
      ans  = fmap unwords <$>
             anagramsM dict (occurrences s)
  return $ evalMemoM ans $ M Map.empty

evalMemoM :: MemoM a -> Memo -> a
evalMemoM = evalState . runMemoT

anagramsM :: Dictionary -> Occurrences -> MemoM [[String]]
anagramsM _ []   = return [[]]
anagramsM dict occr = do
    M memo <- get
    case Map.lookup occr memo of
      Just xs -> return xs
      Nothing -> do
          bs <- forM (filter (/=[]) $ combinations occr) $ \s ->
                  let pres s = maybe [] id $ wordAnagrams s dict
                      append s ys = (:ys) <$> pres s
                  in concatMap (append s) <$>
                     (anagramsM dict $ subtract occr s)
          let as = concat bs
          put $ M $ Map.insert occr as memo
          return as

anagrams :: Dictionary -> Occurrences -> [String]
anagrams d occr = unwords <$> go occr
    where          
        go occr =
            let mkAnagrams s = concatMap (append s) $ suffixes s
                append s t = (:t) <$> prefixes s
                prefixes s = maybe [] id $ wordAnagrams s d
                suffixes s = go $ subtract occr s
                xs         = filter (/=[]) $ combinations occr
            in if null occr then [[]] else concatMap mkAnagrams xs

combinations :: Occurrences -> [Occurrences]
combinations []             = [[]]
combinations ((c, f) : cfs)
    = concatMap extendOccs rest ++ rest
  where
      extendOccs occs = (:occs) <$> prefixs
      rest            = combinations cfs
      prefixs         = (,) c <$> [1 .. f]

subtract :: Occurrences -> Occurrences -> Occurrences
subtract x []              = x
subtract ((xc, xf) : xcfs) y @ ((yc, yf) : ycfs)
    | xc == yc && xf > yf  = (xc, xf - yf) : subtract xcfs ycfs
    | xc == yc && xf == yf = subtract xcfs ycfs
    | otherwise = (xc, xf) : subtract xcfs y

wordAnagrams :: Occurrences -> Dictionary -> Maybe [String]
wordAnagrams occr dict = Map.lookup occr dict

occurrences :: String -> Occurrences
occurrences = map f . group . mkSortedList
  where
      f s = (head s, length s)
      mkSortedList = sort . map toLower . filter (/=' ')

dictionary :: [String] -> Dictionary
dictionary entries = Map.fromList $ map dictEntry xs
  where
      xs = groupBy pairEq $ sortBy (comparing fst) $
           map pairs entries
      dictEntry l = (fst $ head  l, map snd l)
      pairs s = (occurrences s, s)
      pairEq p1 p2 = fst p1 == fst p2
