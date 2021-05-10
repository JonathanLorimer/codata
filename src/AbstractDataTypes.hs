{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
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

-- Using a typeclass v-table
type IsSet rep a = ?setProxy :: Proxy (ClassySet rep a)

class ClassySet rep a where
  cEmpty :: IsSet rep a => rep a
  cIsEmpty :: IsSet rep a => rep a -> Bool
  cInsert :: IsSet rep a => rep a -> a -> rep a
  cContains :: IsSet rep a => rep a -> a -> Bool
  cUnion :: IsSet rep a => rep a -> rep a -> rep a

instance Eq a => ClassySet [] a where
  cEmpty = []
  cIsEmpty = (== [])
  cInsert = flip (:)
  cContains = flip elem
  cUnion = union

runClassySet :: forall a b . Eq a => (forall rep. (IsSet rep a, ClassySet rep a) => b) -> b
runClassySet f = let ?setProxy = Proxy @(ClassySet [] a) in f

test' :: Bool
test' =
  runClassySet @Int $
    cContains (cInsert cEmpty 1 `cUnion` cInsert cEmpty 2) 1
