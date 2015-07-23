module MolSeq where

import Data.List

-----------------------------PART 1-------------------------------
data SeqType = DNA | Protein deriving(Show, Eq)

data MolSeq = MolSeq {seqName1 :: String, seqSequence1 :: String, seqType :: SeqType} deriving(Show, Eq)


string2seq :: String -> String -> MolSeq
string2seq name sequence  	| getType sequence == DNA = MolSeq name sequence DNA
				| otherwise = MolSeq name sequence Protein

--Om alla tecken i sekvensen är ACGT så är sekvensen av typen DNA
getType :: String -> SeqType
getType [] = DNA
getType (s:st) | s `elem` "ACGT"	= getType st
	       | otherwise 		= Protein


seqName (MolSeq n s d) = n
seqSequence (MolSeq n s d) = s
seqLength (MolSeq n s d) = length s


--Om båda är av samma typ beräknas avståndet med respektive formel
seqDistance :: MolSeq -> MolSeq -> Float
seqDistance seq1 seq2 	 	| seqType seq1 == DNA && seqType seq2 == DNA && ((getAlpha seq1 seq2) < 0.74)     	= -(3/4) * log (1 - 4 * (getAlpha seq1 seq2) / 3)
				| seqType seq1 == DNA && seqType seq2 == DNA && ((getAlpha seq1 seq2) > 0.74)		= 3.3
				| seqType seq1 == Protein && seqType seq2 == Protein && ((getAlpha seq1 seq2) <= 0.94) 	= -(19/20) * log (1 - 20 * (getAlpha seq1 seq2) / 19)
				| seqType seq1 == Protein && seqType seq2 == Protein && ((getAlpha seq1 seq2) > 0.94)	= 3.7
			        | otherwise = error "Sequences not of same type"

--Räknar ut alpha-värdet
getAlpha seq1 seq2 = (fromIntegral (getDifference seq1 seq2) / fromIntegral (length (seqSequence seq1)))


getDifference :: MolSeq -> MolSeq -> Integer
getDifference seq1 seq2 = difference (seqSequence seq1) (seqSequence seq2) 0

--Ackumulerande beräkning av antalet positioner som skiljer sig. Om motsvarande position i båda sekvenserna är olika så adderas 1. Terminerar när hela sekvensen gåtts igenom.
difference :: String -> String -> Integer -> Integer
difference [] [] acc = acc
difference (s1:st1) (s2:st2) acc  	| s1 /= s2 	= difference st1 st2 (acc + 1)
					| otherwise 	= difference st1 st2 acc

