----------------------------------------------------------------------
--
-- |
-- Module: Bloxorz
--
--
----------------------------------------------------------------------

module Bloxorz where

import           Bloxorz.GameTerrain (finite)
import           Bloxorz.Skeleton    (Move, startBlock)
import           Bloxorz.Solver      (solution)

level :: [String]
level = [ "|oooSooooo"
        , "|oo----ooo"
        , "|oo---"
        , "|-oooooooo--"
        , "|---oo----oooo"
        , "|oooooooooo"
        , "|oooT----oooo"
        ]

-- | Bloxorz example session
--
-- >>> bloxorz
-- [L,L,D,R,D,R,R,D,L,D]
--
bloxorz :: [Move]
bloxorz = solution terrain goal $ startBlock start
  where
      (terrain, start, goal) = finite level
