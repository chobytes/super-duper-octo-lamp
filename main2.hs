{-# LANGUAGE NoImplicitPrelude
, OverloadedStrings
, GADTs
, KindSignatures
, DataKinds
, PolyKinds
, StandaloneDeriving
, MultiParamTypeClasses
, FlexibleContexts
, FlexibleInstances
, TypeFamilies #-}


import Foundation

class Show n => Numeric n where
  zero :: n
  one :: n
  two :: n
  three :: n
  four :: n
  five :: n
  six :: n
  seven :: n
  eight :: n
  nine :: n

class Eval ty where
  eval :: ty -> ty

class Equality' ty where
  isEq :: ty -> ty -> Boolean

data family Expression ty :: *



-- Nats

data INat :: *
deriving instance Show INat
type Nat = Expression INat

-- instance Numeric (INat) where
--   zero = Z
--   one = S zero
--   two = S one
--   three = S two
--   four = S three
--   five = S four
--   six = S five
--   seven = S six
--   eight = S seven
--   nine = S eight

-- instance Numeric (Nat) where
--   zero = Nat (zero :: INat)
--   one = Nat (one :: INat)
--   two = Nat (two :: INat)
--   three = Nat (three :: INat)
--   four = Nat (four :: INat)
--   five = Nat (five :: INat)
--   six = Nat (six :: INat)
--   seven = Nat (seven :: INat)
--   eight = Nat (eight :: INat)
--   nine = Nat (nine :: INat)

data instance Expression INat where
  Z :: Nat
  S :: Nat -> Nat
--  Nat :: INat -> Nat
  Succ :: Nat -> Nat
  Pred :: Nat -> Nat
  Add :: Nat -> Nat -> Nat
  Sub :: Nat -> Nat -> Nat
  Mult :: Nat -> Nat -> Nat
  Div :: Nat -> Nat -> Nat
deriving instance Show INat => Show (Expression INat)

instance Eval Nat where
  eval Z = Z
  eval a@(S _) = a

  eval (Succ Z) = S Z
  eval (Succ (S a)) = S a

  eval (Pred Z) = Z
  eval (Pred (S a)) = a

  eval (Add t1 t2) = add (eval t1) (eval t2)
    where add :: Nat -> Nat -> Nat
          add a Z = a
          add a (S b) = add (S a) b

  eval (Sub t1 t2) = sub (eval t1) (eval t2)
    where sub :: Nat -> Nat -> Nat
          sub (S a) (S b) = sub a b
          sub a b = a

  eval (Mult t1 t2) = mult (eval t1) (eval t2) Z
    where mult :: Nat -> Nat -> Nat -> Nat
          mult a Z acc = acc
          mult a (S b) acc = mult a b (eval $ Add a acc)

-- instance Equality' Nat where
--   isEq (Nat Z) (Nat Z) = Bool T
--   isEq (Nat (S a)) (Nat (S b)) = isEq (Nat a) (Nat b)
--   isEq _ _ = Bool F



-- Booleans

data IBoolean = T | F deriving Show
type Boolean = Expression IBoolean
data instance Expression IBoolean where
  Bool :: IBoolean -> Boolean
  Or :: Boolean -> Boolean -> Boolean
  And :: Boolean -> Boolean -> Boolean
  Not :: Boolean -> Boolean
  XOr :: Boolean -> Boolean -> Boolean
  NOr :: Boolean -> Boolean -> Boolean
deriving instance Show IBoolean => Show (Expression IBoolean)

instance Equality' Boolean where
  isEq (Bool T) (Bool T) = Bool T
  isEq (Bool F) (Bool F) = Bool T
  isEq _ _ = Bool F

instance Eval Boolean where
  eval a@(Bool _) = a



-- Helpers

evalTerm2 :: Eval t => t -> t -> (t, t)
evalTerm2 t1 t2 = (eval t1, eval t2)



-- For testing purposes
-- interact with functions on Nats using "normal" numbers
-- eg add 2 2 vs (Add (S (S Z)) (S (S Z)))

-- fromNat (Nat n) = fromNat' n 0
--   where fromNat' :: INat -> Int -> Int
--         fromNat' Z acc = acc
--         fromNat' (S a) acc = fromNat' a (acc + 1)

-- fromInt :: Int -> Nat
-- fromInt n = fromInt' n Z
--   where fromInt' :: Int -> INat -> Nat
--         fromInt' 0 acc = Nat acc
--         fromInt' a acc = fromInt' (a - 1) (S acc)

-- natExp :: (Nat -> Nat -> Nat) -> Int -> Int -> Int
-- natExp f a b = fromNat . eval $ f (fromInt a) (fromInt b)

-- add :: Int -> Int -> Int
-- add = natExp Add

-- mult :: Int -> Int -> Int
-- mult = natExp Mult

-- sub :: Int -> Int -> Int
-- sub = natExp Sub





data IEq a = Equal a | NotEqual a deriving Show
type Equality a = Expression (IEq a)
data instance Expression (IEq a) where
  Eq :: a -> a -> Equality a
deriving instance (Show a, Show (IEq a)) => Show (Expression (IEq a))

-- instance Eval (Equality Nat) where
--   eval (Eq (Nat a) (Nat b)) = case (a, b) of
--     (Z, Z) -> undefined


data IBoolean' :: * -> * where
type Boolean' a = Expression (IBoolean' a)
data instance Expression (IBoolean' a) where
  T' :: a -> b -> Boolean' a
  F' :: a -> b -> Boolean' b
