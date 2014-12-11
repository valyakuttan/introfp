-- |
-- Module: Set

module Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , contains
    , notContains

      -- * Set operations
    , delete
    , difference
    , insert
    , intersection
    , union

      -- * Functional operations
    , filter
    , map

      -- * Bulk operations
    , fromList
    ) where


import           Prelude hiding (filter, map)

-- $setup
-- >>> import Control.Applicative ((<$>))
-- >>> import Test.QuickCheck
-- >>> instance (Arbitrary a, Eq a) => Arbitrary (Set a) where arbitrary = fromList <$> arbitrary
-- >>> instance Show a => Show (Set a) where show _ = "Set"


-- | A newtype wrapper for a Set
-- A Set is represented by its characteristic function
newtype Set a = Set { cf :: a -> Bool }

-- | Check whether the set contains element.
--
-- Examples:
--
-- >>> let s = Set $ (== 1)
-- >>> contains s 1
-- True
-- >>> contains s 0
-- False
contains :: Set a -> a -> Bool
contains = cf

-- | Check whether the set does not contains element.
notContains :: Set a -> a -> Bool
notContains (Set f) = not . f

-- | The empty set.
--
-- prop> contains empty e == False
empty :: Set a
empty = Set $ const False

-- | Create a singleton set.
--
-- prop> contains (singleton x) y == (x == y)
singleton :: Eq a => a -> Set a
singleton a = Set (== a)

-- | The union of two sets.
--
-- prop> contains (union s1 empty) e == contains s1 e
-- prop> contains (union s1 $ singleton e) e == True
union :: Set a -> Set a -> Set a
union s t = Set $ \a -> contains s a || contains t a

-- | The intersection of two sets.
--
-- >>> let s1 = fromList [1..5]
-- >>> let s2 = fromList [3..7]
-- >>> let s  = s1 `intersection` s2
-- >>> and $ fmap (contains s) [1..7]
-- False
--
-- >>> and $ fmap (contains s) [3..5]
-- True
intersection :: Set a -> Set a -> Set a
intersection s t = Set $ \a -> contains s a && contains t a

-- | The difference of two sets.
--
-- >>> let s1 = fromList [1..5]
-- >>> let s2 = fromList [3..7]
-- >>> let s  = s1 `difference` s2
-- >>> and $ fmap (contains s) [1..7]
-- False
--
-- >>> and $ fmap (contains s) [1,2]
-- True
difference :: Set a -> Set a -> Set a
difference s t = Set $ \a -> contains s a && not (contains t a)

-- | Insert an element in a set.
--
-- >>> let s = insert 0 $ fromList [1..5]
-- >>> and $ fmap (contains s) [0..5]
-- True
-- >>> and $ fmap (contains s) [0..6]
-- False
insert :: Eq a => a -> Set a -> Set a
insert = union . singleton

-- | Delete an element from set.
--
-- >>> let s = delete 5 $ fromList [1..5]
-- >>> and $ fmap (contains s) [1..4]
-- True
-- >>> and $ fmap (contains s) [1..5]
-- False
--
-- >>> let s = delete 0 $ fromList [1..5]
-- >>> and $ fmap (contains s) [1..4]
-- True
-- >>> and $ fmap (contains s) [1..5]
-- True
delete :: Eq a => a -> Set a -> Set a
delete = flip difference . singleton

-- | Filter all elements that satisfy the predicate.
--
-- >>> let s = filter (> 5) $ fromList [1..10]
-- >>> and $ fmap (contains s) [6..10]
-- True
-- >>> and $ fmap (contains s) [5..10]
-- False
filter :: (a -> Bool) -> Set a -> Set a
filter p s = Set $ \a -> contains s a && p a

-- | map f s is the set obtained by applying f to each element of s.
map :: (b -> a) -> Set a -> Set b
map f (Set g) = Set $ g . f

-- | Construct a set which contains all elements in the list.
--
-- prop> let checker = contains (fromList xs) in and (fmap checker xs) == True
fromList :: Eq a => [a] -> Set a
fromList = foldr insert empty
