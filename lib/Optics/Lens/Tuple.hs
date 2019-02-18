{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Optics.Lens.Tuple where

import Optics.Base
import Optics.Lens
import Optics.Iso

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> a where
  _1 :: Lens s t a b

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> a where
  _2 :: Lens s t a b

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> a where
  _3 :: Lens s t a b

-- Instances for 2-tuples
instance Field1 (a, x) (b, x) a b where
  _1 = first'

instance Field2 (x, a) (x, b) a b where
  _2 = second'

-- Instances for 3-tuples
instance Field1 (a, x, y) (b, x, y) a b where
  _1 = isoToLens (iso (\(a,x,y) -> (a,(x,y)))
                      (\(b,(x,y)) -> (b,x,y)))

instance Field2 (x, a, y) (x, b, y) a b where
  _2 = isoToLens (iso (\(x,a,y) -> (a,(x,y)))
                      (\(b,(x,y)) -> (x,b,y)))

instance Field3 (x, y, a) (x, y, b) a b where
  _3 = isoToLens (iso (\(x,y,a) -> (a,(x,y)))
                      (\(b,(x,y)) -> (x,y,b)))