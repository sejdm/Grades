{-# LANGUAGE FlexibleInstances #-}

module SemiVectorSpace (SemiVectorSpace (..)) where

import Data.Monoid

class Monoid a => SemiVectorSpace a where
  (#*) :: Double -> a -> a

instance SemiVectorSpace b => SemiVectorSpace (a -> b) where
  t #* f = \x -> t #* f x

instance (SemiVectorSpace a, SemiVectorSpace b) => SemiVectorSpace (a, b) where
  t #* (x, y) = (t #* x, t #* y)

instance SemiVectorSpace (Sum Double) where
  t #* (Sum x) = Sum (t * x)
