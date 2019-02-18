module Optics.Traversal where

import Data.Profunctor.Traversing

import Optics.Base

type Traversal s t a b = forall p. Traversing p => p a b -> p s t

traversed :: Traversable f => Traversal (f a) (f b) a b
traversed = traverse'

traverseOf :: Applicative f => Optic (Star f) s t a b -> (a -> f b) -> s -> f t
traverseOf l k = runStar (l (Star k))