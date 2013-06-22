-- Solver.hs
--
--

module Bloxorz.Solver 
    (
      position
    , solution
    , startBlock
    ) where

import Bloxorz.Skeleton

solution :: Terrain -> Position -> Block -> [Move]
solution t goal start =
    case pathsToGoal goal start t of
        []             -> []
        (_, moves) : _ -> reverse moves

pathsToGoal :: Position -> Block -> Terrain -> [(Block, [Move])]
pathsToGoal goal b t = filter (done goal . fst) $ pathsFromStart b t

pathsFromStart :: Block -> Terrain -> [(Block, [Move])]
pathsFromStart start t = from [(start, [])] [start] t

from :: [(Block, [Move])] -> [Block] -> Terrain -> [(Block, [Move])]
from []                  _        _ = []
from (bh@(b, h) : rest) explored t  = bh : from rest' explored' t
  where
      rest'     = rest ++ nbs
      explored' = explored ++ map fst nbs
      nbs       = newNeighbors explored $ neighborsWithHistory b h t

newNeighbors :: [Block] -> [(Block, [Move])] -> [(Block, [Move])]
newNeighbors _        []                 = []
newNeighbors explored (nb@(b, _) : rest)
    | b `elem` explored = newNeighbors explored rest
    | otherwise = nb : newNeighbors (b : explored) rest

neighborsWithHistory :: Block -> [Move] -> Terrain -> [(Block, [Move])]
neighborsWithHistory b history t = map f $ legalNeighbors b t
    where
        f = combineWith (,) fst $ (:history) . snd

combineWith :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
combineWith f g h v = f (g v) (h v)

