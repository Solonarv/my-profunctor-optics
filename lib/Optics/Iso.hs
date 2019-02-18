{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Optics.Iso where

import Data.Coerce
import Data.Type.Equality

import Data.Profunctor.Unsafe
import Optics.Base

type Iso s t a b = forall p. Profunctor p => p a b -> p s t

type Iso' s a = Iso s s a a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso t f = dimap t f

_Refl' :: (s :~: a) -> Iso' s a
_Refl' Refl = id

_Coerce :: (Coercible s a, Coercible b t) => Iso s t a b
_Coerce pab = coerce #. pab .# coerce

_Coerce' :: Coercible s a => Iso' s a
_Coerce' = _Coerce