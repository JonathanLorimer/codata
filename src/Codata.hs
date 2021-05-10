{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Codata where

import Prelude hiding (Bool(..))

data DataBool = True | False

newtype If = If { unIf :: forall a. a -> a -> a }

next :: a -> b -> b
next _ b = b

true, false :: If
true = If const
false = If next

ite :: If -> a -> a -> a
ite (If bool) = bool

class CodataBool b where
  ifThenElse :: forall a. b -> a -> a -> a

instance CodataBool DataBool where
  ifThenElse :: DataBool -> a -> a -> a
  ifThenElse True = const
  ifThenElse False = next

data DataTree a = Leaf a | Branch (DataTree a) (DataTree a)

-- | codata as a function
dataWalk :: forall a b. (a -> b) -> (b -> b -> b) -> DataTree a -> b
dataWalk leaf _ (Leaf a) = leaf a
dataWalk leaf branch (Branch l r) = branch (dataWalk leaf branch l) (dataWalk leaf branch r)

-- | codata as a typeclass
class CodataTree tree a b where
  coLeaf :: a -> b
  coBranch :: b -> b -> b
  coWalk :: tree a -> b

instance Show a => CodataTree DataTree a String where
  coLeaf :: a -> String
  coLeaf = show

  coBranch :: String -> String -> String
  coBranch = (<>)

  coWalk :: DataTree a -> String
  coWalk = dataWalk (coLeaf @DataTree) (coBranch @DataTree @a)


