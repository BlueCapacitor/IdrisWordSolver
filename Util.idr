module Util

import Data.SortedMap
import Data.Zippable

export
countEqual : (Foldable t, Eq a) => a -> t a -> Nat
countEqual e l = count (== e) l

public export
(Show k, Show v) => Show (SortedMap k v) where
	show = show . SortedMap.toList

public export
all2 : (Foldable t, Zippable t) => (a -> b -> Bool) -> t a -> t b -> Bool
all2 f xs ys = and $ zipWith (\x, y => Delay (f x y)) xs ys

public export
any2 : (Foldable t, Zippable t) => (a -> b -> Bool) -> t a -> t b -> Bool
any2 f xs ys = or $ zipWith (\x, y => Delay (f x y)) xs ys
