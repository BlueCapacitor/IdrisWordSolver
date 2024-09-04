module Letters

import Data.SortedMap
import Data.String
import Data.Vect

import Util
import MultiMap
import SetLike


alphabetStr : String
alphabetStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

export
numLetters : Nat
numLetters = length alphabetStr

export
alphabet : Vect Letters.numLetters Char
alphabet = fromList $ unpack alphabetStr

public export
LetterVect : Type
LetterVect = Vect Letters.numLetters Int8

public export
SetLike LetterVect where
	union = zipWith (+)
	-- difference = zipWith minus
	difference = zipWith (-)
	-- symDifference = zipWith (\a, b => minus a b + minus b a)
	symDifference = zipWith (\a, b => abs (a - b))
	intersection = zipWith min
	isEmpty = all (== 0)
	isSuperset = all2 (>=)
	isSubset = all2 (<=)
	isStrictSuperset = all2 (>)
	isStrictSubset = all2 (<)
	isEquivalent = (==)

public export
LetterMap : Type
LetterMap = MultiMap LetterVect String

export
getLetters : String -> LetterVect
getLetters word = cast <$> (flip countEqual (toUpper <$> unpack word) <$> alphabet)

export
buildLetterMap : List String -> LetterMap
buildLetterMap dictionary = MultiMap.fromList $ produceEntry <$> dictionary where
	produceEntry : String -> (LetterVect, String)
	produceEntry word = (getLetters word, word)

