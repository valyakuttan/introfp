{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
--
-- |
-- Module: Bloxorz.Skeleton
--
--
-- x - coordinate denotes position on vertical axis
-- y - coordinate denotes poistion on horizontal axis
--
-- Illustration:
--
--        0 1 2 3 4 5 6 7 8 9  <- y axis
--      0 o o o o o o o o o o
--      1 o o o # o o o o o o  # is at position (1, 3)
--      2 o o o o o o o o o o
--      3 o o o o o o o o o o
--      4 o o o o o o o o o o
--      5 o o o o o o o o $ o  $ is at position (5, 8)
--      6 o o o o o o o o o o
--      7 o o o o o o o o o o
--      8 o o o o o o o o o o
--      9 o o o o o o o o o o
--
--      ^
--      |
--
--      x axis
--
----------------------------------------------------------------------

module Bloxorz.Skeleton
    (
      Block
    , Move
    , Position
    , Terrain
    , done
    , legalNeighbors
    , position
    , startBlock
    , x
    , y
    ) where


import           Control.Lens


data Position = Position
    { _x :: !Int
    , _y :: !Int
    } deriving (Eq, Show)

makeLenses ''Position

type Terrain = Position -> Bool

data Move = L | R | U | D deriving (Show)

data Block = Block
    { _b1 :: !Position
    , _b2 :: !Position
    } deriving (Eq, Show)

makeLenses ''Block

startBlock :: Position -> Block
startBlock s = Block s s

position :: Int -> Int -> Position
position = Position

done :: Position -> Block -> Bool
done goal b = standing b && b^.b1 == goal

legalNeighbors :: Block -> Terrain -> [(Block, Move)]
legalNeighbors b t = filter (isLegal t . fst)  neighbors
  where
    neighbors = [ (left b, L), (right b, R)
                , (up b, U),   (down b, D)
                ]
    isLegal terrain (Block p1 p2)
      = legal p1 && legal p2 && terrain p1 && terrain p2
    legal  p = p^.x >= 0 && p^.y >= 0

left :: Block -> Block
left b | standing b = b & b1.y -~ 2 & b2.y -~ 1
       | vertical b = b & b1.y -~ 1 & b2.y -~ 2
       | otherwise  = b & b1.y -~ 1 & b2.y -~ 1

right :: Block -> Block
right b | standing b = b & b1.y +~ 1 & b2.y +~ 2
        | vertical b = b & b1.y +~ 2 & b2.y +~ 1
        | otherwise  = b & b1.y +~ 1 & b2.y +~ 1

up :: Block -> Block
up b | standing b = b & b1.x -~ 2 & b2.x +~ 1
     | vertical b = b & b1.x -~ 1 & b2.x -~ 1
     | otherwise  = b & b1.x -~ 1 & b2.x -~ 2

down :: Block -> Block
down b | standing b = b & b1.x +~ 1 & b2.x +~ 2
       | vertical b = b & b1.x +~ 1 & b2.x +~ 1
       | otherwise  = b & b1.x +~ 2 & b2.x +~ 1

standing :: Block -> Bool
standing b = b^.b1 == b^.b2

vertical :: Block -> Bool
vertical b = b^.b1.x == b^.b2.x
