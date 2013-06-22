-- PurelyFunctionalSet.hs
--

module PurelyFunctionalSet 
       ( contains
       , empty
       , singleton
       , union
       , intersection
       , difference
       , filter
       , map 
       ) where

import Prelude hiding (filter, map)

newtype Set a = Set {cf :: a -> Bool}    -- A Set is represented by its
                                         -- characteristic function.

contains :: Set a -> a -> Bool
contains = cf

empty :: Set a
empty = Set $ const False

singleton :: Eq a => a -> Set a
singleton a = Set (== a)

union :: Set a -> Set a -> Set a
union s t = Set $ \a -> contains s a || contains t a

intersection :: Set a -> Set a -> Set a
intersection s t = Set $ \a -> contains s a && contains t a

difference :: Set a -> Set a -> Set a
difference s t = Set $ \a -> contains s a && not (contains t a)


filter :: (a -> Bool) -> Set a -> Set a
filter p s = Set $ \a -> contains s a && p a

map :: (b -> a) -> Set a -> Set b
map f (Set g) = Set $ g . f
