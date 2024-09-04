module Main

import System.File
import Data.String
import Data.SortedMap
import Data.SortedSet
import Data.Vect

import Letters
import Util
import MultiMap
import SetLike


dictionaryFilePath : String
dictionaryFilePath = "wlist_match12_narrower.txt"

target : String
target = "ganpaprloee"

anagram : List String -> String -> List String
anagram dictionary = anagramRec . getLetters where
	letterMap : LetterMap
	letterMap = buildLetterMap dictionary

	letterVects : List LetterVect
	letterVects = SortedSet.toList $ SortedSet.keySet letterMap

	mutual
		anagramRec : LetterVect -> List String
		anagramRec target = recWord target =<< letterVects

		recWord : LetterVect -> LetterVect -> List String
		recWord target letters =
			if isSuperset target letters then
				if isSubset target letters then
					SortedSet.toList $ MultiMap.lookup letters letterMap
				else
					(flip (++) . (" " ++)) <$>
						(SortedSet.toList $ MultiMap.lookup letters letterMap) <*>
							anagramRec (difference target letters)
			else
				[]



solve : List String -> String
solve dictionary =
	show $ anagram dictionary target
	-- show $ isSuperset target (getLetters "apple")


main : IO ()
main = do
	putStrLn "Start"
	readResult <- readFile dictionaryFilePath
	putStrLn "Read"
	case readResult of
		Left err => putStrLn $ "File Read Error:\n" ++ show err
		Right content => do
			let dictionary = lines content
			putStrLn "Split"
			let solution = solve dictionary
			putStrLn "Solved"
			putStrLn solution
	putStrLn "Done"
