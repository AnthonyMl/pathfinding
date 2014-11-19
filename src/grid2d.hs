
{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: remember to limit what we export
module Grid2D where


import Data.List    as List
import Data.HashSet as Set

import Graph
import Vec2




data Grid2D = Grid2D {
  size      :: Int,
  start     :: Vec2,
  goal      :: Vec2,
  obstacles :: HashSet Vec2
  }

create :: Int -> Vec2 -> Vec2 -> Grid2D
create size start goal = Grid2D size start goal Set.empty

addObstacles :: Grid2D -> [Vec2] -> Grid2D
addObstacles (Grid2D size start goal obstacles) newObstacles = Grid2D size start goal (Set.union obstacles (Set.fromList newObstacles))

data Action = Up | Down | Left | Right deriving (Eq, Enum)

value :: Action -> Vec2
value Grid2D.Up    = Vec2   0    1
value Grid2D.Down  = Vec2   0  (-1)
value Grid2D.Left  = Vec2 (-1)   0
value Grid2D.Right = Vec2   1    0

apply :: Vec2 -> Action -> Vec2
apply vec action = vec + value action

inBounds :: Grid2D -> Vec2 -> Action -> Bool
inBounds grid (Vec2 _ y) Grid2D.Up    = y < Grid2D.size grid
inBounds _    (Vec2 _ y) Grid2D.Down  = y >= 0
inBounds _    (Vec2 x _) Grid2D.Left  = x >= 0
inBounds grid (Vec2 x _) Grid2D.Right = x < Grid2D.size grid

isValid :: Grid2D -> Vec2 -> Action -> Bool
isValid grid state action =
    inBounds grid newState action && not (Set.member newState (Grid2D.obstacles grid))
  where
    newState = apply state action


instance Graph Grid2D Vec2 where
  getNeighbors g state = List.map (apply state) (List.filter (isValid g state) [Grid2D.Up .. Grid2D.Right])
  start                = Grid2D.start
  goal                 = Grid2D.goal






