module Optics.Prism where

import Optics.Base
import Optics.Iso

type Prism s t a b = forall p. Choice p => p a b -> p s t

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism match re pab = dimap match (either id re) (right' pab)

prism' :: (s -> Maybe a) -> (b -> s) -> Prism s s a b
prism' match = prism (\s -> case match s of Nothing -> Left s; Just a -> Right a)

isoToPrism :: Iso s t (Either x a) (Either x b) -> Prism s t a b
isoToPrism iso = iso . right'