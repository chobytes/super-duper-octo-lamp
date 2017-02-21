{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}


import Foundation

-- class ListType l where
--   car :: l ty -> ty
--   cdr :: l ty -> ty

-- class Boolean ty where
--   nil :: ty
--   t :: ty

-- class Comparable ty where
--   eq :: Boolean b => ty -> ty -> b

-- data Cons a = Cons a (Cons a)
--             | Nil
--             deriving (Show, Functor)
-- instance ListType Cons where
--   car (Cons a _) = a
--   cdr (Cons _ (Cons a _)) = a
-- instance Boolean Cons where
--   nil = Nil
--   t =

-- -- newtype Atom = Atom [Char] deriving (Show)
-- -- instance Comparable Atom where
-- --   eq (Atom []) (Atom (y:ys)) = nil
-- --   eq (Atom (x:xs)) (Atom []) = nil
-- --   eq (Atom [x]) (Atom [y]) = t
-- --   eq (Atom (x:xs))  (Atom (y:ys)) | x /= y = nil
-- --                                   | otherwise = eq (Atom xs) (Atom ys)
-- data family Lisp a :: *

-- data Equality
-- data instance Lisp Equality where
--   Eq :: Lisp a -> Lisp a -> Lisp Equality

-- data Atom
-- data instance Lisp Atom where
--   Atom :: [Char] -> Lisp Atom

-- data Quote
-- data instance Lisp Quote where
--   Quote :: Lisp a -> Lisp Quote

-- data Lambda
-- data instance Lisp Lambda where
--   Lambda :: (Lisp a -> Lisp b) -> Lisp a -> Lisp Lambda

-- data Cons a
-- data instance Lisp (Cons a) where
--   Cons :: Lisp a -> Cons a -> Lisp (Cons a)
--   Nil :: Lisp (Cons a)

-- data T = T deriving Show
-- data instance Lisp T




-- eval (Eq (Atom []) (Atom [x])) = Nil
-- eval (Eq (Atom [x]) (Atom [])) = Nil
-- eval (Eq (Atom [x]) (Atom [y])) | x == y = T
-- eval (Eq (Atom (x:xs)) (Atom (y:ys))) | x /= y = Nil
--                                       | otherwise = eval (Eq (Atom xs) (Atom ys))
data Nat = S Nat | Z
data Lisp = Atom [Char]
          | List [Lisp]
          | Number Nat
          | Bool Bool
          | Quote Lisp
          | Lambda Lisp Lisp
          | Eq Lisp Lisp
