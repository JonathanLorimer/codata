{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module IndexedCodata where

import Prelude

-- basic indexed data encoding
data Nat = Zero | Succ Nat

data Vector (n :: Nat) a where
  Nil :: Vector 'Zero a
  Cons :: a -> Vector n a -> Vector ('Succ n) a

-- Basic indexed  church encoding
-- The key is that each construction takes a stack and returns some value
class CoStack stack a where
  pop :: stack ('Succ n) a -> (a, stack n a)
  push :: stack n a -> a -> stack ('Succ n) a

instance CoStack Vector Int where
  pop (x `Cons` xs) = (x, xs)
  push = flip Cons

data ChurchStack1 s n a where
  ChurchPop :: (s ('Succ n) a -> (a, s n a)) -> ChurchStack1 s n a
  ChurchPush ::  (s n a -> a -> s ('Succ n) a) -> ChurchStack1 s n a

data ChurchStack2 s n a where
  MkChurchStack :: (s ('Succ n) a -> (a, s n a)) -> (s n a -> a -> s ('Succ n) a) -> ChurchStack2 s n a

concreteChurchStack1 :: ChurchStack1 Vector n a
concreteChurchStack1 = ChurchPop \(x `Cons` xs) -> (x, xs)

concreteChurchStack1' :: ChurchStack1 Vector n a
concreteChurchStack1' = ChurchPush $ flip Cons

concreteChurchStack2 :: ChurchStack2 Vector n a
concreteChurchStack2 = MkChurchStack (\(x `Cons` xs) -> (x, xs)) (flip Cons)

-- Associated type families
class AbstractStack where
  type ConcreteStack (n :: Nat) a
  pop' :: ConcreteStack ('Succ n) a -> (a, ConcreteStack n a)
  push' :: ConcreteStack n a -> a -> ConcreteStack ('Succ n) a

instance AbstractStack where
  type ConcreteStack n a = Vector n a
  pop' (x `Cons` xs) = (x, xs)
  push' = flip Cons
