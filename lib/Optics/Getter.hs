{-# LANGUAGE QuantifiedConstraints #-}
module Optics.Getter where

import Data.Coerce

import Data.Functor.Contravariant

import Optics.Base
import Orphans.Profunctors

type Getter s t a b = forall p. (Profunctor p, forall x. Contravariant (p x)) => p a b -> p s t

type Getting r s t a b = Optic (Forget r) s t a b

view :: Getting a s t a b -> s -> a
view l = runForget (l (Forget id))

(^.) :: s -> Getting a s t a b -> a
(^.) = flip view

to :: (s -> a) -> Getter s t a b
to get = (() >$) . dimap get (const ())