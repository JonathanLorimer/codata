{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module IndexedCodata where

import Prelude
import Data.Proxy

-- basic indexed data encoding
data Nat = Zero | Succ Nat

data Vector (n :: Nat) a where
  Nil :: Vector 'Zero a
  Cons :: a -> Vector n a -> Vector ('Succ n) a

-- Basic indexed codata encoding
class CoStack s a where
  pop :: s ('Succ n) a -> (a, s n a)
  push :: s n a -> a -> s ('Succ n) a

instance CoStack Vector Int where
  pop (x `Cons` xs) = (x, xs)
  push = flip Cons

-- Associated type families
class AbstractStack a where
  type ConcreteStack (n :: Nat) a
  pop' :: ConcreteStack ('Succ n) a -> (a, ConcreteStack n a)
  push' :: ConcreteStack n a -> a -> ConcreteStack ('Succ n) a

instance AbstractStack Int where
  type ConcreteStack n Int = Vector n Int
  pop' (x `Cons` xs) = (x, xs)
  push' = flip Cons

-- Encoding codata within data
data CoEncStack1 n a where
  MkS :: (Proxy ('Succ n ~ n) -> (a, CoEncStack1 n a))
      -> (CoEncStack1 n a -> a -> CoEncStack1 ('Succ n) a)
      -> CoEncStack1 n a

data CoEncStack2 n a =
  CoEncStack2
    (Proxy ('Succ n) -> (a, CoEncStack2 n a))
    (CoEncStack2 n a -> a -> CoEncStack2 ('Succ n) a)

data CoEncStack3 s n a where
  Pop :: (s ('Succ n) a -> (a, s n a)) -> CoEncStack3 s n a
  Push :: (s n a -> a -> s ('Succ n) a) -> CoEncStack3 s n a

