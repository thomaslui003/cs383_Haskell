{-# LANGUAGE InstanceSigs #-}
import Distribution.Simple.Utils (xargs)
import Data.Time.Format.ISO8601 (yearFormat)

-- Student number: 301310026
-- Student name: Wen Luo Thomas Lui
-- CMPT 383 Homework 3

-- to zip 2 list, used (Cons x xs) (Cons y ys) to get the x and y element and put them into a tuple and repeat with (listZip xs ys)
data List a = Empty | Cons a (List a ) deriving (Eq, Ord, Show, Read)

listZip :: List a -> List b  -> List (a,b) 
listZip Empty x = Empty
listZip j Empty = Empty
listZip (Cons x xs) (Cons y ys) = Cons (x,y) (listZip xs ys)


-- if k new node value is smaller than node x value then insert to left subtree else insert to the right subtree if bigger
-- lastly, if it's not any of them, it is equal to insert itself with k valued node.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert k EmptyTree = Node k EmptyTree EmptyTree
insert k (Node x left right)
  | k < x = Node x (insert k left) right
  | k > x = Node x left (insert k right) 
  | otherwise = Node k EmptyTree EmptyTree
  


data Nat a = Zero | Succ (Nat a) deriving (Show)

natPlus :: Nat a -> Nat a -> Nat a 
natPlus Zero x = x
natPlus y Zero = y
natPlus (Succ a) b = Succ (natPlus a b)

-- natMult used the natPlus function as derived from (m + 1) · n = m · n + n.
natMult :: Nat a -> Nat a -> Nat a 
natMult Zero x = Zero
natMult y Zero = Zero
natMult (Succ a) b = natPlus (natMult a b) b
 
-- included the case for emptytree comparison where all true cases are considered in order for False to work
instance Eq a => Eq (Tree a)  where
    EmptyTree == EmptyTree = True
    (Node a b c) == (Node x y z) = a == x && b == y && c == z -- = True also work
    _ == _ = False

--in the first fmap, a means key , b means value, and c is the (AssocList k v).
-- Then we want f to represent the operator function and apply the operator function on the b which is the value as well 
-- as all the other ones with (fmap f c) recursively
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where
    --fmap :: (a->b) -> AssocList a  -> AssocList c
    fmap f (ALCons a b c) = ALCons a (f b) (fmap f c)
    fmap _ ALEmpty = ALEmpty


