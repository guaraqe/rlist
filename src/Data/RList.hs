{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.RList
       (
       -- * The `RList` type
         RList
       -- * Basic functions
       , fromList
       , toList
       , snoc ) where

import Control.Applicative

newtype RList a = RList [a]
                  deriving (Eq, Ord, Functor, Applicative)

instance Show a => Show (RList a) where
  show (RList l) = show (reverse l)

instance Alternative RList where
  empty = RList []
  (RList l1) <|> (RList l2) = RList (l2 <|> l1)

fromList :: [a] -> RList a
fromList = RList . reverse

toList :: RList a -> [a]
toList (RList l) = reverse l

snoc :: RList a -> a -> RList a
snoc (RList xs) x = RList (x:xs)
