module Optics.Lens where

import Control.Arrow ((&&&))

import Optics.Base
import Optics.Iso

type Lens s t a b = forall p. Strong p => p a b -> p s t

lens :: (s -> (a, b -> t)) -> Lens s t a b
lens decomp pab = dimap decomp (\(b,bt) -> bt b) (first' pab)

lens' :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens' get set pab = dimap (id &&& get) (uncurry set) (second' pab)

isoToLens :: Iso s t (a, x) (b, x) -> Lens s t a b
isoToLens iso = iso . first'