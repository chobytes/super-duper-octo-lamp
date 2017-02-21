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



class Eval ty where
  eval :: ty -> ty

data family Expression ty :: *



-- Nats

data INat :: *
deriving instance Show INat
type Nat = Expression INat

data instance Expression INat where
  Z :: Nat
  S :: Nat -> Nat
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



-- Boolean

data IBoolean :: *
deriving instance Show IBoolean
type Boolean = Expression (IBoolean)

data instance Expression (IBoolean) where
  T :: Boolean
  F :: Boolean
  Or :: Boolean -> Boolean -> Boolean
  And :: Boolean -> Boolean -> Boolean
  Not :: Boolean -> Boolean
  XOr :: Boolean -> Boolean -> Boolean
  NAnd :: Boolean -> Boolean -> Boolean
deriving instance Show IBoolean => Show (Expression IBoolean)

instance Eval Boolean where
  eval (Or T T) = T
  eval (Or T _) = T
  eval (Or _ T) = T
  eval (Or _ _) = F
  eval (And T T) = T
  eval (And _ _) = F
  eval (Not T) = F
  eval (Not F) = T
  eval (XOr T F) = T
  eval (XOr F T) = T
  eval (XOr _ _) = F
  eval (NAnd F F) = T
  eval (NAnd _ _) = F



-- Helpers

natExp :: (Nat -> Nat -> Nat) -> Int -> Int -> Int
natExp f a b = fromNat . eval $ f (fromInt a) (fromInt b)

add :: Int -> Int -> Int
add = natExp Add

mult :: Int -> Int -> Int
mult = natExp Mult

sub :: Int -> Int -> Int
sub = natExp Sub

fromNat :: Nat -> Int
fromNat n = fromNat' n 0
  where fromNat' :: Nat -> Int -> Int
        fromNat' Z acc = acc
        fromNat' (S a) acc = fromNat' a (acc + 1)

fromInt :: Int -> Nat
fromInt n = fromInt' n Z
  where fromInt' :: Int -> Nat -> Nat
        fromInt' 0 acc = acc
        fromInt' a acc = fromInt' (a - 1) (S acc)
