module Orphans.Profunctors where

import Data.Coerce
import Data.Functor.Contravariant

import Data.Profunctor

instance Contravariant (Forget r a) where
  contramap _ = coerce

instance Contravariant f => Contravariant (Star f a) where
  contramap f (Star o) = Star (contramap f . o)