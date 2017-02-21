{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

data Tree ty where
  Leaf :: ty -> Tree ty
  Node :: Tree ty -> Tree ty -> Tree ty
deriving instance Show ty => Show (Tree ty)

treeHeight :: (Num n, Ord n) => Tree ty -> n
treeHeight (Leaf _) = 1
treeHeight (Node l r)
  = let lHeight = 1 + treeHeight l
        rHeight = 1 + treeHeight r
    in max lHeight rHeight
