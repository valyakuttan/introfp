{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------
--
-- |
-- Module: Pouring
--
--------------------------------------------------

module Pouring  where


import           Control.Lens
import           Control.Monad.State
import           Data.Set            (Set)
import qualified Data.Set            as S


-- | Datatype representing glass.
data Glass = Glass
    { _amount   :: !Int -- ^ Amount of liquid in it
    , _capacity :: !Int -- ^ Capacity of glass
    } deriving (Eq, Ord)

makeLenses ''Glass

type G = (Int, Glass)

-- | Datatype representing move
data Move = Empty !G     -- ^ Glass which is emptied
          | Fill  !G     -- ^ Glass which is filled
          | Pour  !G !G  -- ^ Pour from source to destination

-- | Datatype represent the whole system
data Path = Path
    { _history :: ![Move]      -- ^ History of the system
    , _glasses     :: ![Glass] -- ^ Current state
    }

makeLenses ''Path

type S = ([Path], Set Path)

-- | Pouring
--
pourings :: Int -> [Int] -> Maybe Path
pourings goal capacities = evalState pourings' start
  where
    pourings' = use _1 >>= go
    go (p:_) | goalState p = return $ Just p
             | otherwise   = update >> pourings'
    go _                   = return Nothing
    goalState = anyOf (glasses . traverse . amount) (== goal)
    start = ([path], S.empty)
    path     = Path [] $ fmap (Glass 0) capacities

update :: State S ()
update = do
    paths <- use _1

    unless (null paths) $ do
        explored <- use _2

        let (p:ps) = paths
            unexplored  = (`S.notMember` explored)
            nextLevel q = map ( extend q) $ possibleMoves $ q ^. glasses
            extend (Path hs gs) m = Path (m : hs) $ change m gs

        if unexplored p
            then do
                _1 .= ps ++ filter unexplored (nextLevel p)
                _2 .= S.insert p explored
            else
                _1 .= ps

possibleMoves :: [Glass] -> [Move]
possibleMoves s = emptys ++ fills ++ pours
  where
      is   = [0..] `zip` s
      emptys = fmap Empty is
      fills  = fmap Fill is
      pours  = [Pour x y | x <- is, y <- is, x /= y]

change :: Move -> [Glass] -> [Glass]
change (Empty (i,g))        s = s & ix i .~ empty g
change (Fill (i,g))         s = s & ix i .~ fill g
change (Pour (i,g1) (j,g2)) s = s & ix i .~ g1' & ix j .~ g2'
  where (g1',g2') = pour g1 g2

pour :: Glass -> Glass -> (Glass, Glass)
pour s d = (s & amount -~ w, d & amount +~ w)
  where
      w  = min (s ^. amount) (d ^. capacity  - d ^. amount)

empty :: Glass -> Glass
empty = amount .~ 0

fill :: Glass -> Glass
fill g = g & amount .~ (g ^. capacity)

instance Show Path where
    show (Path hs _) = show (reverse hs)

instance Eq Path where
    Path _ s == Path _ t = s == t

instance Ord Path where
  compare (Path _ s) (Path _ t) = compare s t

instance Show Move where
    show (Empty (_,g))         = show g ++ " -> " ++ show (empty g)
    show (Fill  (_,g))         = show g ++ " -> " ++ show (fill g)
    show (Pour  (_,g1) (_,g2)) = show g ++ " -> "++ show g'
      where
        g  = (g1,g2)
        g' = pour g1 g2

instance Show Glass where
    show (Glass a c) = "(" ++ show a ++ "|" ++ show c ++ ")"
