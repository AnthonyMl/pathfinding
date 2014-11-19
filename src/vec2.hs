
{-# LANGUAGE DeriveGeneric #-}


module Vec2 where

import GHC.Generics (Generic)
import Data.Hashable as Hashable


data Vec2 = Vec2 { x :: Int, y :: Int } deriving (Show, Eq, Generic)

instance Num Vec2 where
  Vec2 x1 y1 + Vec2 x2 y2 = Vec2       (x1 + x2)       (y1 + y2)
  Vec2 x1 y1 - Vec2 x2 y2 = Vec2       (x1 - x2)       (y1 - y2)
  Vec2 x1 y1 * Vec2 x2 y2 = Vec2       (x1 * x2)       (y1 * y2)
  negate       (Vec2 x y) = Vec2            (-x)            (-y)
  abs          (Vec2 x y) = Vec2         (abs x)        (abs  y)
  signum       (Vec2 x y) = Vec2      (signum x)      (signum y)
  fromInteger           i = Vec2 (fromInteger i) (fromInteger i)

instance Hashable Vec2

dot :: Vec2 -> Vec2 -> Int
dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

fromList :: Functor f => f (Int, Int) -> f Vec2
fromList xs = fmap (uncurry Vec2) xs



