{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
module Codata where

import Prelude hiding (Bool(..))

data DataBool = True | False

class CodataBool b where
  ifThenElse :: forall a. b -> a -> a -> a

instance CodataBool DataBool where
  ifThenElse :: DataBool -> a -> a -> a
  ifThenElse True = const
  ifThenElse False = seq

data DataTree a = Leaf a | Branch (DataTree a) (DataTree a)

-- | codata as a function
dataWalk :: forall a b. (a -> b) -> (b -> b -> b) -> DataTree a -> b
dataWalk leaf _ (Leaf a) = leaf a
dataWalk leaf branch (Branch l r) = branch (dataWalk leaf branch l) (dataWalk leaf branch r)

-- | codata as a typeclass
class CodataTree t a b where
  coLeaf :: a -> b
  coBranch :: b -> b -> b
  coWalk :: t a -> b

instance Show a => CodataTree DataTree a String where
  coLeaf :: a -> String
  coLeaf = show

  coBranch :: String -> String -> String
  coBranch = (<>)

  coWalk :: DataTree a -> String
  coWalk = dataWalk (coLeaf @DataTree) (coBranch @DataTree @a)


