----------------------------------------------------------------------
--
-- |
-- Module: Bloxorz.GameTerrain
--
--
----------------------------------------------------------------------

module Bloxorz.GameTerrain
    (
      infinite
    , finite
    ) where


import           Bloxorz.Skeleton (Position, Terrain, position, x, y)
import           Control.Lens


infinite :: (Int, Int) -> (Int, Int) -> (Terrain, Position, Position)
infinite start goal = (const True, pos start, pos goal)
  where
      pos = uncurry position

finite :: [String] -> (Terrain, Position, Position)
finite terrainMap = (stringToTerrain ms, start, goal)
  where
      ms      = stripMargin chMargin terrainMap
      start   = findPosition chStart ms
      goal    = findPosition chGoal ms

stringToTerrain :: [String] -> Terrain
stringToTerrain ms = tf
    where
        tf :: Position -> Bool
        tf p = validIndex x' ms && validIndex y' (ms!!x')
             && ms!!x'!!y' /= '-'
          where
          x' = p^.x
          y' = p^.y
        validIndex i xs   = 0 <= i && i < length xs

stripMargin :: Char -> [String] -> [String]
stripMargin margin = map dropMargin
  where
      dropMargin s
          | margin `elem` s = tail $ dropWhile (/=margin) s
          | otherwise = s

findPosition :: Char -> [String] -> Position
findPosition c ms = position (f xs) (f ys)
  where
      f  = fst . head
      ys = filter ((== c) . snd) $ zip [0 ..] $ snd $ head xs
      xs = filter ((c `elem`) . snd) $ zip [0 ..] ms

-- | Character legend in terrain map
--
chMargin, chStart, chGoal :: Char
chMargin = '|'
chStart  = 'S'
chGoal   = 'T'
