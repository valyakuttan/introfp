-- Skeleton.hs
--
--
-- This is an attempt to translate some material offered in a
-- MOOC by Martin Odersky to Haskell.
--
--
-- x - coordinate denotes the position on the vertical axis
-- y - coordinate is used for the horizontal axis
-- the coordinates increase when moving down and right
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

module Skeleton 
    (
      Block
    , Move
    , Position ( x, y)
    , Terrain
    , done
    , legalNeighbors
    , position
    , startBlock
    ) where

data Position = Position
    { x :: Int
    , y :: Int
    } deriving (Eq, Show)

type Terrain = Position -> Bool

data Move = L | R | U | D deriving (Show)

data Block = Block Position Position deriving (Eq, Show)

startBlock :: Position -> Block
startBlock s = Block s s

done :: Position -> Block -> Bool
done goal b@(Block p1 _) = isStanding b && p1 == goal

legalNeighbors :: Block -> Terrain -> [(Block, Move)]
legalNeighbors b t = filter (isLegal t . fst)  $ neighbors b

position :: Int -> Int -> Position
position = Position

isStanding :: Block -> Bool
isStanding (Block p1 p2) = p1 == p2

neighbors :: Block -> [(Block, Move)]
neighbors b = [ (left b, L), (right b, R)
              , (up b, U), (down b, D) ]

left :: Block -> Block
left b@(Block (Position x1 _) (Position x2 _))
    | isStanding b = bdy (-2) (-1) b
    | x1 == x2     = bdy (-1) (-2) b
    | otherwise    = bdy (-1) (-1) b

right :: Block -> Block
right b@(Block (Position x1 _) (Position x2 _))
    | isStanding b = bdy 1 2 b
    | x1 == x2     = bdy 2 1 b
    | otherwise    = bdy 1 1 b


up :: Block -> Block
up b@(Block (Position x1 _) (Position x2 _))
    | isStanding b = bdx (-2) (-1) b
    | x1 == x2     = bdx (-1) (-1) b
    | otherwise    = bdx (-1) (-2) b


down :: Block -> Block
down b@(Block (Position x1 _) (Position x2 _))
    | isStanding b = bdx 1 2 b
    | x1 == x2     = bdx 1 1 b
    | otherwise    = bdx 2 1 b

isLegal :: Terrain -> Block -> Bool
isLegal terrain (Block p1 p2)
    = legal p1 && legal p2 && terrain p1 && terrain p2

bdx :: Int -> Int -> Block -> Block
bdx d1 d2 (Block p1 p2) = Block (dx d1 p1) (dx d2 p2)

bdy :: Int -> Int -> Block -> Block
bdy d1 d2 (Block p1 p2) = Block (dy d1 p1) (dy d2 p2)

dx :: Int -> Position -> Position
dx d (Position x y) = position (x + d) y

dy :: Int -> Position -> Position
dy d (Position x y) = position x (y + d)

legal :: Position -> Bool
legal (Position x y) = x >= 0 && y >= 0
