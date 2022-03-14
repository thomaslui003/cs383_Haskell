{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
import Distribution.Simple.Utils (xargs)
import GHC.Num

-- Student number: 301310026
-- Student name: Wen Luo Thomas Lui
-- CMPT 383 Homework 4

--q1 simple functor 
data ErrJst e j = Err e | Jst j deriving (Show)

instance Functor (ErrJst e) where

    fmap f (Jst j) = Jst (f j)
    fmap _ (Err e) = Err e

--q2 ErrJst e an applicative functor 

instance Applicative (ErrJst e) where
    pure = Jst
    (Err e ) <*> _ = Err e
    (Jst f) <*> x = fmap f x

--q3 ErrJst e a monad 

instance Monad (ErrJst e) where
    (Err e) >>= _ = Err e
    (Jst x) >>= f = f x

--q4 used id to implement 

join :: (Monad m) => m (m a) -> m a
join x =  x >>= id


--q5 LTree as an instance of Foldable and it mappend both left and right monoid values and join them into a single monoid value.

data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)

instance Foldable LTree where
    --foldMap :: (a -> m) -> LTree a -> m
    foldMap f (Leaf l)= f l
    foldMap f (LNode a b) = mappend (foldMap f a)(foldMap f b)


