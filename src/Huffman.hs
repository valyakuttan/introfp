-- |
-- Module: Huffman

module Huffman
    (
      CodeTree
    , Bit(L, R)
    , codeTree
    , encode
    , decode
    ) where


import           Data.Function (on)
import           Data.List     (group, sort, sortBy)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Ord      (comparing)


data CodeTree = Fork !CodeTree !CodeTree !String !Integer
              | Leaf !String !Integer

chars :: CodeTree -> String
chars (Fork _ _ cs _) = cs
chars (Leaf cs _)     = cs

weight :: CodeTree -> Integer
weight (Fork _ _ _ w) = w
weight (Leaf _ w)     = w

instance Show CodeTree where
    show (Fork l r cs _)
        = cs ++ " ( " ++ show l ++ " " ++ show r ++ " ) "
    show (Leaf cs _) = "( " ++ cs ++ " )"

data Bit = L | R

instance Show Bit where
    show L = "0"
    show R = "1"

newtype CodeTable = CodeTable (Map Char [Bit])

leaf :: Char -> Integer -> CodeTree
leaf c = Leaf [c]

mkCodeTree :: CodeTree -> CodeTree -> CodeTree
mkCodeTree l r = Fork l r cs w
  where
    cs = on (++) chars l r
    w  = on (+) weight l r

-- $setup
-- >>> import Test.QuickCheck


-- | Convert a string to a list of @Leaf@s sorted by their weight.
--
-- prop> let xs = map weight (leaves s) in xs == sort xs
leaves :: String -> [CodeTree]
leaves = map (uncurry leaf) . sortBy (comparing snd) . chwtPairs
  where
    chwtPairs = toPairs . group . sort
    toPairs :: [String] -> [(Char, Integer)]
    toPairs (xs@(x:_):xxs) = (x, toInteger $ length xs) : toPairs xxs
    toPairs (_:xxs)        = toPairs xxs
    toPairs []             = []

-- | Insert a @CodeTree@ into a list of @CodeTree@s, sorted
-- by their weight.
--
-- prop> let {c1 = leaf c w; xs = leaves s; ws = map weight (insCodeTree c1 xs)} in ws == sort ws
insCodeTree :: CodeTree -> [CodeTree] -> [CodeTree]
insCodeTree ct cts = ls ++ [ct] ++ rs
  where (ls,rs) = span (\ct' -> weight ct' < weight ct) cts

-- | Construct a Huffman tree from a list of subtrees.
combine :: [CodeTree] -> Maybe CodeTree
combine [ct]              = Just ct
combine (ct1 : ct2 : cts) = let ct'  = mkCodeTree ct1 ct2
                                cts' = ct' `seq` insCodeTree ct' cts
                            in combine $! cts'
combine _                 = Nothing

-- | Generate Huffman tree for an alphabet.
codeTree :: String -> Maybe CodeTree
codeTree = combine . leaves

-- | Recover a string from its Huffman representation.
-- >>> let msg1       = "helloworld"
-- >>> let (Just ct1) = codeTree msg1
-- >>> let (Just zt1) = encode ct1 msg1
-- >>> decode ct1 zt1 == msg1
-- True
--
-- >>> let msg2       = replicate 10 'a'
-- >>> let (Just ct2) = codeTree msg2
-- >>> let (Just zt2) = encode ct2 msg2
-- >>> decode ct2 zt2 == msg2
-- True
decode :: CodeTree -> [Bit] -> String
decode (Leaf [c] _) bs = replicate (length bs) c
decode ct           bs = decode' ct bs
  where
      decode' (Leaf c _)     bs'       = c ++ decode' ct bs'
      decode' (Fork l _ _ _) (L : bs') = decode' l bs'
      decode' (Fork _ r _ _) (R : bs') = decode' r bs'
      decode' _              []        = []

-- | construct a map of letter to its code from the tree.
mkCodeTable :: CodeTree -> CodeTable
mkCodeTable (Leaf cs _) = CodeTable $ Map.fromList [(head cs, [L])]
mkCodeTable ct = CodeTable $ Map.fromList $ mkCodeTable' ct []
  where
      mkCodeTable' (Leaf cs _)    bs = [(head cs, bs)]
      mkCodeTable' (Fork l r _ _) bs =
        mkCodeTable' l (bs ++ [L]) ++ mkCodeTable' r (bs ++ [R])

-- | Encode a string using Huffman code.
encode :: CodeTree -> String -> Maybe [Bit]
encode ct = fmap concat . mapM encoder
  where
      encoder = code $ mkCodeTable ct
      code (CodeTable t) = flip Map.lookup t
