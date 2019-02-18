module Optics.Review where

import Data.Bifunctor

import Data.Tagged

import Optics.Base
import Optics.Getter

type Review s t a b = forall p. (Profunctor p, Bifunctor p) => p a b -> p s t

type AReview s t a b = Optic Tagged s t a b

unto :: (b -> t) -> Review s t a b
unto g = rmap g . lphantom

un :: Getting a s t a b -> Review b a t s
un l = unto (view l)

lphantom :: (Profunctor p, Bifunctor p) => p x b -> p y b
lphantom = lmap (const ()) . first (const ())