module MultiMap

import Data.SortedMap
import Data.SortedSet

import SetLike

public export
MultiMap : Type -> Type -> Type
MultiMap k v = SortedMap k (SortedSet v)

export
lookup : (Ord v) => k -> MultiMap k v -> (SortedSet v)
lookup key multiMap =
	case SortedMap.lookup key multiMap of
		Just sortedSet => sortedSet
		Nothing => SortedSet.empty

export
insert : (Ord v) => k -> v -> MultiMap k v -> MultiMap k v
insert key value curr = SortedMap.insert key (
	case SortedMap.lookup key curr of
		Just sortedSet => insert value sortedSet
		Nothing => SortedSet.singleton value
	) curr

export
insertFrom : (Foldable f, Ord v) => f (k, v) -> MultiMap k v -> MultiMap k v
insertFrom entries curr = foldl (\multiMap, (key, value) => insert key value multiMap) curr entries

export
fromList : (Ord k, Ord v) => List (k, v) -> MultiMap k v
fromList = flip insertFrom SortedMap.empty

export
mergeWith : (Ord v) => (SortedSet v -> SortedSet v -> SortedSet v) -> MultiMap k v -> MultiMap k v -> MultiMap k v
mergeWith f a b = SortedMap.mergeWith f (mergeLeft a (b $> SortedSet.empty)) (mergeLeft b (a $> SortedSet.empty))

(Ord v) => SetLike (MultiMap k v) where
	union = SortedMap.mergeWith SetLike.union
	difference a b = SortedMap.mergeWith SetLike.difference (SortedMap.mergeLeft a (b $> SortedSet.empty)) b
	symDifference = SortedMap.mergeWith SetLike.symDifference
	isEmpty = all isEmpty
