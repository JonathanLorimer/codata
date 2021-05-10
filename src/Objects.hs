{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Objects where

import Prelude

data Shape where
  MkShape :: Shapely a => a -> Shape

class Shapely s where
  area :: s -> Float
  perimeter :: s -> Float

newtype Circle = Circle { radius :: Float }

instance Shapely Circle where
  area (Circle r) = pi * (r*r)
  perimeter (Circle r) = 2 * pi * r

newtype Square = Square { width :: Float }

instance Shapely Square where
  area (Square w) = w * w
  perimeter (Square w) = 4 * w

data Rect = Rect { rWidth :: Float, rHeight :: Float }

instance Shapely Rect where
  area (Rect w h ) = w * h
  perimeter (Rect w h) = (w * 2) + (h * 2)

shapeList :: [Shape]
shapeList = [MkShape (Circle 4), MkShape (Square 2), MkShape (Rect 2 4)]
