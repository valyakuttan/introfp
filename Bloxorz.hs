-- Bloxorz.hs
--
-- 

module Bloxorz where

import Bloxorz.GameTerrain
import Bloxorz.Solver
import Bloxorz.Skeleton ( Move )

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

