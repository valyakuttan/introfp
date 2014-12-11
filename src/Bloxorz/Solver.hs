----------------------------------------------------------------------
--
-- |
-- Module: Bloxorz.Solver
--
--
----------------------------------------------------------------------

module Bloxorz.Solver
    ( solution
    , startBlock
    ) where


import           Control.Applicative ((<$>))
import           Bloxorz.Skeleton


solution :: Terrain -> Position -> Block -> [Move]
solution t goal start =
    case pathsToGoal goal start t of
        []             -> []
        (_, moves) : _ -> reverse moves

pathsToGoal :: Position -> Block -> Terrain -> [(Block, [Move])]
pathsToGoal goal b t = filter (done goal . fst) $ pathsFromStart b t

pathsFromStart :: Block -> Terrain -> [(Block, [Move])]
pathsFromStart start = from [(start, [])] [start]

from :: [(Block, [Move])] -> [Block] -> Terrain -> [(Block, [Move])]
from []                  _        _ = []
from (bh@(b, h) : rest) explored t  = bh : from rest' explored' t
  where
      rest'     = rest ++ nbs
      explored' = explored ++ map fst nbs
      nbs       = next explored $ fmap (:h) <$> legalNeighbors b t
      next _ []                 = []
      next expld (n@(m, _) : ns)
        | m `elem` expld = next expld ns
        | otherwise = n : next (m : expld) ns
