{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuantifiedConstraints #-}
module AbstractDataTypes where

import Prelude
import Data.List (union)
import Data.Proxy
import Data.Kind

-- Module based implementation
newtype ModuleSet a = ModuleSet { unModuleSet :: [a] }

moduleSetEmpty :: ModuleSet a
moduleSetEmpty = ModuleSet []

moduleSetIsEmpty :: ModuleSet a -> Bool
moduleSetIsEmpty (ModuleSet []) = True
moduleSetIsEmpty _ = False

moduleSetInsertEmpty :: ModuleSet a -> a -> ModuleSet a
moduleSetInsertEmpty (ModuleSet xs) x = ModuleSet $ x : xs

moduleSetContains :: Eq a => ModuleSet a -> a -> Bool
moduleSetContains (ModuleSet xs) x = x `elem` xs

moduleSetUnion :: Eq a => ModuleSet a -> ModuleSet a -> ModuleSet a
moduleSetUnion (ModuleSet a) (ModuleSet b) = ModuleSet $ a `union` b

-- Existential based implementation
data ExistentialSet a where
  MkSet :: forall (rep :: Type -> Type) a.
    rep a ->
    (rep a -> Bool) ->
    (rep a -> a -> rep a) ->
    (rep a -> a -> Bool) ->
    (rep a -> rep a -> rep a) ->
    ExistentialSet a

setImp :: Eq a => ExistentialSet a
setImp =
  MkSet
    []            -- empty
    (== [])       -- isEmpty
    (flip (:))    -- insert
    (flip elem)   -- contains
    union         -- union

runSetImp :: Eq a => (ExistentialSet a -> b) -> b
runSetImp f = f setImp

test :: Bool
test  =
  runSetImp @Int \(MkSet empty _ insert contains unionImp) ->
    contains (insert empty 1 `unionImp` insert empty 2) 1

-- Explicit Classy Example
data List a = Nil | a `Cons` List a

class ListOps t where
  nil :: t a
  adjoin :: a -> t a -> t a
  null :: t a -> Bool
  headMay :: t a -> Maybe a
  tailMay :: t a -> Maybe (t a)
  equal :: Eq a => t a -> t a -> Bool

instance ListOps List where
  nil = Nil
  adjoin = Cons
  null = \case Nil -> True; _ -> False
  headMay = \case Nil -> Nothing; x `Cons` _ -> Just x
  tailMay = \case Nil -> Nothing; _ `Cons` xs -> Just xs
  equal Nil Nil = True
  equal (x`Cons`xs) (y`Cons`ys) = x == y && xs `equal` ys
  equal _ _ = False

testListOps :: (forall l. ListOps l, Num a, Eq a) => t a -> Bool
testListOps l = equal (1 `adjoin` nil) (1 `adjoin` (2 `adjoin` (3 `adjoin` l)))

class ClassySet rep where
  cEmpty :: rep a
  cIsEmpty :: rep a -> Bool
  cInsert :: rep a -> a -> rep a
  cContains :: Eq a => rep a -> a -> Bool
  cUnion :: Eq a => rep a -> rep a -> rep a

instance ClassySet [] where
  cEmpty = []
  cIsEmpty = \case [] -> True; _ -> False;
  cInsert = flip (:)
  cContains = flip elem
  cUnion = union

runClassySet :: (forall rep. ClassySet rep => rep a -> b) -> b
runClassySet f = f []

testClassySet :: Bool
testClassySet =
  runClassySet @Int $ \s ->
    cContains (cInsert s 1 `cUnion` cInsert s 2) 1

-- Using only typeclass v-table
type IsSet rep a = ?setProxy :: Proxy (VTableSet rep a)

class VTableSet rep a where
  vEmpty :: IsSet rep a => rep a
  vIsEmpty :: IsSet rep a => rep a -> Bool
  vInsert :: IsSet rep a => rep a -> a -> rep a
  vContains :: IsSet rep a => rep a -> a -> Bool
  vUnion :: IsSet rep a => rep a -> rep a -> rep a

instance Eq a => VTableSet [] a where
  vEmpty = []
  vIsEmpty = (== [])
  vInsert = flip (:)
  vContains = flip elem
  vUnion = union

runVTableSet :: forall a b . Eq a => (forall rep. (IsSet rep a, VTableSet rep a) => b) -> b
runVTableSet f = let ?setProxy = Proxy @(VTableSet [] a) in f

testVTableSet :: Bool
testVTableSet =
  runVTableSet @Int $
    vContains (vInsert vEmpty 1 `vUnion` vInsert vEmpty 2) 1


