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

data family Vec :: INat -> * -> *
data instance Vec n Int where
  Nil :: Vec Z Int
  Cons :: Int -> Vec n Int -> Vec (S n) Int

class (Show t) => Expression t where
  data Exp t
  eval :: Exp t -> Exp t

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

data INat = Z | S INat deriving Show
type Nat = Exp INat
instance Numeric (INat) where
  zero = Z
  one = S zero
  two = S one
  three = S two
  four = S three
  five = S four
  six = S five
  seven = S six
  eight = S seven
  nine = S eight

instance Numeric (Nat) where
  zero = Nat (zero :: INat)
  one = Nat (one :: INat)
  two = Nat (two :: INat)
  three = Nat (three :: INat)
  four = Nat (four :: INat)
  five = Nat (five :: INat)
  six = Nat (six :: INat)
  seven = Nat (seven :: INat)
  eight = Nat (eight :: INat)
  nine = Nat (nine :: INat)

-- type Nat = Nat
instance Expression INat where
  data Exp INat where
    Nat :: INat -> Nat
    Succ :: Nat -> Nat
    Pred :: Nat -> Nat
    Add :: Nat -> Nat -> Nat
    Sub :: Nat -> Nat -> Nat
    Mult :: Nat -> Nat -> Nat
    Div :: Nat -> Nat -> Nat

  eval a@(Nat _) = a

  eval (Succ a)
    = let (Nat a') = eval a
      in Nat $ S a'

  eval (Pred a)
    = let (Nat a') = eval a
          eval' x = case x of
            Z -> Z
            (S x') -> x'
      in Nat$ eval' a'

  eval (Add a b)
    = let (Nat a') = eval a
          (Nat b') = eval b
          eval' x y = case y of
            Z -> x
            (S y') -> eval' (S x) (y')
      in Nat $ eval' a' b'

  eval (Sub a b)
    = let (Nat a') = eval a
          (Nat b') = eval b
          eval' x y = case (x,y) of
            (Z,_) -> x
            (_,Z) -> x
            ((S x'),(S y')) -> eval' x' y'
      in Nat $ eval' a' b'

  eval (Mult a b)
    = let a' = eval a
          b' = eval b
          acc = Nat Z
          eval' x y z = case y of
            (Nat Z) -> z
            (Nat (S _)) -> eval' x (eval $ Sub y (Nat (S Z))) (eval $ Add x z)
      in eval' a' b' acc

  -- TODO: "Works" but is limited by current subractions functions, and Z being the bottom of Nat
  -- example of problem: 9 / 2 = 5, 9 / 4 = 3
  -- Solution?: check if sub result is Z, and if the numebrs are the same (the only time i should be Z)
  -- Solution?: with future compare function -> refactor to add denom to denom until denom is great than numer, accumulate the occurences
  eval (Div a b)
    = let a' = eval a
          b' = eval b
          acc = Z
          eval' x y z = case (x, y) of
            (Nat Z, _) -> z
            (_, Nat Z) -> undefined
            (Nat (S _), Nat (S _)) -> eval' (eval $ Sub x y) y (S z)
            _ -> undefined
      in Nat $ eval' a' b' acc
deriving instance Show INat => Show (Exp INat)

data IBoolean = True' | False' deriving Show
type Boolean = Exp IBoolean
instance Expression IBoolean where
  data Exp IBoolean where
    Bool :: IBoolean -> Boolean
    And :: Boolean -> Boolean -> Boolean
    Or :: Boolean -> Boolean -> Boolean
    Not :: Boolean -> Boolean -> Boolean

  eval a@(Bool _) = a

  eval (Or t1 t2)
    = let (Bool t1') = eval t1
          (Bool t2') = eval t2
          eval' a b = case (a, b) of
            (False', False') -> False'
            _ -> True'
      in Bool $ eval' t1' t2'

  eval (And t1 t2)
    = let (Bool t1') = eval t1
          (Bool t2') = eval t2
          eval' a b = case (a, b) of
            undefined -> undefined
      in undefined
deriving instance Show IBoolean => Show (Exp IBoolean)

fromENat :: Nat -> INat
fromENat (Nat a) = a

fromNat :: INat -> Int
fromNat n = case n of
  Z -> 0
  (S a) -> 1 + (fromNat a)

exp' :: (Nat -> Nat -> Nat) -> Nat -> Nat -> Int
exp' f a b = fromNat . fromENat . eval $ f a b

add :: Nat -> Nat -> Int
add = exp' Add

mult :: Nat -> Nat -> Int
mult = exp' Mult

sub :: Nat -> Nat -> Int
sub = exp' Sub

div :: Nat -> Nat -> Int
div = exp' Div
