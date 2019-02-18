module Optics.Base
  ( module Data.Profunctor
  , Optic, Optic', Simple
  ) where

import Data.Profunctor

type Optic p s t a b = p a b -> p s t

type Optic' p s a = p a a -> p s s

type Simple o s a = o s s a a