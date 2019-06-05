module Minicity.Point where

import Data.Eq (Eq)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Ord (Ord)
import Lens.Micro (Lens')
import Prelude (Num, (*), (+), abs, fromInteger, negate, signum)

data Point =
  Point Int Int
  deriving (Eq, Ord)

pointBinOp :: (Int -> Int -> Int) -> Point -> Point -> Point
pointBinOp f (Point x0 y0) (Point x1 y1) = Point (f x0 x1) (f y0 y1)

pointOp :: (Int -> Int) -> Point -> Point
pointOp f (Point x y) = Point (f x) (f y)

instance Num Point where
  (+) = pointBinOp (+)
  (*) = pointBinOp (*)
  abs = pointOp abs
  signum = pointOp signum
  negate = pointOp negate
  fromInteger x = Point (fromInteger x) (fromInteger x)

_x :: Lens' Point Int
_x f (Point x y) = (`Point` y) <$> f x

_y :: Lens' Point Int
_y f (Point x y) = (`Point` y) <$> f x
