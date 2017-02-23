{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
 
module ConstrainedStuff
       (
         Functor (..)
       , (<$>)
       ) where

import GHC.Exts
import qualified Prelude as P
import Control.Monad.Trans.State.Lazy
import Data.Map.Lazy


class Functor f where
     type FunctorConstraint f x :: Constraint
     type FunctorConstraint f x = ()

     fmap :: (FunctorConstraint f a, FunctorConstraint f b) => (a -> b) -> f a -> f b

 -- | An infix synonym for 'fmap'.
(<$>) :: (Functor f, FunctorConstraint f a, FunctorConstraint f b) => (a -> b) -> f a -> f b
(<$>) = fmap


instance Functor [] where fmap = P.fmap
instance P.Monad m => Functor (StateT s m) where fmap = P.fmap
instance P.Ord k => Functor (Map k) where fmap = P.fmap
