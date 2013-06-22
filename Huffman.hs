--Huffman.hs
--
--

module Huffman
    (
       CodeTree
     , Bit(L, R)
     , codeTree
     , encode
     , decode
     ) where

import Data.List (sort, group, sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map

data CodeTree = Fork
    { left   :: CodeTree
    , right  :: CodeTree
    , chars  :: String
    , weight :: Integer
    }
    | Leaf
    { chars  :: String
    , weight :: Integer
    }

instance Show CodeTree where
    show (Fork l r cs _)
        = cs ++ " ( " ++ show l ++ " " ++ show r ++ " ) "
    show (Leaf cs _) = "( " ++ cs ++ " )"

data Bit = L | R

instance Show Bit where
    show L = "0"
    show R = "1"

newtype CodeTable = CodeTable (Map.Map Char [Bit])

leaf :: Char -> Integer -> CodeTree
leaf c w = Leaf [c] w

mkCodeTree :: CodeTree -> CodeTree -> CodeTree
mkCodeTree l r = Fork l r cs w
  where
      cs = chars l ++ chars r
      w  = weight l + weight r

leaves :: String -> [CodeTree]
leaves = map mkLeaf . sortedps . sortedcs
  where
      sortedcs     :: String -> [String]
      sortedcs     =  group . sort
      ps           =  map $ \cs -> (head cs, toInteger $ length cs)
      sortedps     =  sortBy (comparing snd) . ps
      mkLeaf       =  uncurry leaf

insCodeTree :: CodeTree -> [CodeTree] -> [CodeTree]
insCodeTree ct []        = [ct]
insCodeTree ct (ct' : cts)
            | weight ct == weight ct' = ct : ct' : cts
            | otherwise = ct' : insCodeTree ct cts

combine :: [CodeTree] -> CodeTree
combine [ct]              = ct
combine (ct1 : ct2 : cts) = combine cts'
  where
      ct'  = mkCodeTree ct1 ct2
      cts' = insCodeTree ct' cts

codeTree :: String -> CodeTree
codeTree = combine . leaves

decode :: CodeTree -> [Bit] -> String
decode (Leaf [c] _) bs = take (length bs) $ repeat c
decode ct           bs = decode' ct bs
  where
      decode' (Leaf c _)     bs'       = c ++ decode' ct bs'
      decode' (Fork l _ _ _) (L : bs') = decode' l bs'
      decode' (Fork _ r _ _) (R : bs') = decode' r bs'
      decode' _              []        = []

code :: CodeTable -> Char -> [Bit]
code (CodeTable ct) c = maybe' $ Map.lookup c ct
  where
      maybe' = maybe (error $ "unknown character : " ++ [c]) id

mkCodeTable :: CodeTree -> CodeTable
mkCodeTable ct = CodeTable $ Map.fromList $ mkCodeTable' ct []
  where
      mkCodeTable' (Leaf [c] _)   bs = [(c, bs)]
      mkCodeTable' (Fork l r _ _) bs
          = mkCodeTable' l (bs ++ [L]) ++ mkCodeTable' r (bs ++ [R])

encode :: CodeTree -> String -> [Bit]
encode ct s = concatMap encoder s
  where
      encoder = code $ mkCodeTable ct