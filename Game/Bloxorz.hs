-- Bloxorz.hs
--
-- 

module Bloxorz where

import GameTerrain
import Solver
import Skeleton ( Move )

level :: [String]
level = [ "|oooSooooo"
       , "|oo----ooo"
       , "|oo---"
       , "|-oooooooo--"
       , "|---oo----oooo"
       , "|oooooooooo"
       , "|oooT----oooo" ]

bloxorz :: [Move]
bloxorz = solution terrain goal $ startBlock start
  where
      (terrain, start, goal) = finite level

