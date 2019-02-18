module Optics.Fold where

import Data.Monoid

import Optics.Base
import Optics.Getter

foldMapOf :: Monoid m => Getting m s t a b  -> (a -> m) -> s -> m
foldMapOf l f = runForget (l (Forget f))

toListOf :: Getting (Endo [a]) s t a b -> s -> [a]
toListOf l s = appEndo (foldMapOf l (Endo . (:)) s) []

(^..) :: s -> Getting (Endo [a]) s t a b -> [a]
(^..) = flip toListOf

safeHeadOf :: Getting (First a) s t a b -> s -> Maybe a
safeHeadOf l = getFirst . foldMapOf l (First . Just)

(^?) :: s -> Getting (First a) s t a b -> Maybe a
(^?) = flip safeHeadOf