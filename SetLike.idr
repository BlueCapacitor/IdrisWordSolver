module SetLike

import Data.SortedSet
import Data.List

public export
interface SetLike t where
	union : t -> t -> t

	difference : t -> t -> t

	symDifference : t -> t -> t
	symDifference a b = SetLike.union (SetLike.difference a b) (SetLike.difference b a)

	intersection : t -> t -> t
	intersection a b = SetLike.difference (SetLike.union a b) (SetLike.symDifference a b)

	isEmpty : t -> Bool

	isSubset : t -> t -> Bool
	isSubset a b = SetLike.isEmpty $ SetLike.difference a b

	isSuperset : t -> t -> Bool
	isSuperset a b = SetLike.isEmpty $ SetLike.difference b a

	isStrictSubset : t -> t -> Bool
	isStrictSubset a b = SetLike.isSubset a b && not (SetLike.isSuperset a b)

	isStrictSuperset : t -> t -> Bool
	isStrictSuperset a b = SetLike.isSuperset a b && not (SetLike.isSubset a b)

	isEquivalent : t -> t -> Bool
	isEquivalent a b = SetLike.isEmpty $ SetLike.symDifference a b


public export
SetLike (SortedSet k) where
	union = SortedSet.union
	difference = SortedSet.difference
	symDifference = SortedSet.symDifference
	intersection = SortedSet.intersection
	isEmpty = isNil . SortedSet.toList

public export
(SetLike t) => Semigroup (t) where
	(<+>) = SetLike.union

