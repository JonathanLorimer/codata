{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Objects where

import Prelude hiding (null, tail)
import Data.Maybe

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

data ListObject a =
  ListObject
    { null :: Bool
    , headMay :: Maybe a
    , tailMay :: Maybe (ListObject a)
    , cons :: a -> ListObject a
    , equal :: Eq a => ListObject a -> Bool
    }

instance Show a => Show (ListObject a) where
  show lo = if null lo then "Nil" else (show . fromJust $ headMay lo) <> " : " <> (show . fromJust $ tailMay lo)

nil :: ListObject a
nil =
  let self =
        ListObject
          { null = True
          , headMay = Nothing
          , tailMay = Nothing
          , cons = (`cell` self)
          , equal = null
          }
  in self

cell :: a -> ListObject a -> ListObject a
cell x l =
  let self =
        ListObject
          { null = False
          , headMay = Just x
          , tailMay = Just l
          , cons = (`cell` self)
          , equal = \m -> Just x == headMay m && (case tailMay m of
                                                    Nothing -> null l
                                                    Just tail -> equal l tail)
          }
  in self

infixr 5 `cell`

test :: ListObject Int
test = 1 `cell` 2 `cell` 3 `cell` nil

-- $> Objects.null Objects.test
--
-- $> Objects.headMay Objects.test
--
-- $> Objects.tailMay Objects.test
--
-- $> Objects.headMay Objects.test
--
-- $> Objects.equal Objects.test Objects.test
--
-- $> Objects.equal Objects.test Objects.nil


data ListObjectExtended a =
  ListObjectExtended
    { nullEx :: Bool
    , headMayEx :: Maybe a
    , tailMayEx :: Maybe (ListObjectExtended a)
    , consEx :: a -> ListObjectExtended a
    , equalEx :: Eq a => ListObjectExtended a -> Bool
    }

instance Show a => Show (ListObjectExtended a) where
  show lo = if nullEx lo then "Nil" else (show . fromJust $ headMayEx lo) <> " : " <> (show . fromJust $ tailMayEx lo)

nilEx :: ListObjectExtended a
nilEx =
  let self =
        ListObjectExtended
          { nullEx = True
          , headMayEx = Nothing
          , tailMayEx = Nothing
          , consEx = (`cellEx` self)
          , equalEx = nullEx
          }
  in self

cellEx :: a -> ListObjectExtended a -> ListObjectExtended a
cellEx x l =
  let self =
        ListObjectExtended
          { nullEx = False
          , headMayEx = Just x
          , tailMayEx = Just l
          , consEx = (`cellEx` self)
          , equalEx = \m -> Just x == headMayEx m && (case tailMayEx m of
                                                    Nothing -> nullEx l
                                                    Just tail -> equalEx l tail)
          }
  in self

intervalEx :: (Num a, Ord a) => a -> a -> ListObjectExtended a
intervalEx x y =
  let self =
        ListObjectExtended
          { nullEx = False
          , headMayEx = Just x
          , tailMayEx = Just $ intervalEx (x + 1) y
          , consEx = (`cellEx` self)
          , equalEx =
            \m -> Just x == headMayEx m
               && (case tailMayEx m of
                        Nothing -> headMayEx m == Just y
                        Just tail -> equalEx (intervalEx (x + 1) y) tail)
          }
   in if x > y then nilEx else self

infixr 5 `cellEx`

testEx :: ListObjectExtended Int
testEx = 1 `cellEx` 2 `cellEx` 3 `cellEx` nilEx
