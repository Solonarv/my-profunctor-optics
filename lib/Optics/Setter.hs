module Optics.Setter where

type Setter s t a b = (a -> b) -> s -> t

modify, (%~) :: Setter s t a b -> (a -> b) -> s -> t
modify = id
(%~) = modify

set, (.~) :: Setter s t a b -> b -> s -> t
set l b = modify l (const b)
(.~) = set