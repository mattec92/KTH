module Evol where

import Data.List
import MolSeq
import Profile

class Evol a where
      distance :: a -> a -> Float
      name :: a -> String

instance Evol MolSeq where
         distance a b = seqDistance a b
         name a = seqName a

instance Evol Profile where
         distance a b = profileDistance a b
         name a = profileName a

--Går igenom hela listan med profiler/molseq, anropar distanceMatrixHelp som beräknar avståndet mellan huvudet och resterande del av listan.
distanceMatrix :: (Eq a, Evol a) => [a] -> [(String, String, Float)]
distanceMatrix list | list == [] = []
                    | otherwise = distanceMatrixHelp (head list) (tail list) ++ distanceMatrix (tail list)

--Beräknar avståndet mellan ett element och alla andra element i given lista.
distanceMatrixHelp :: (Eq a, Evol a) => a -> [a] -> [(String, String, Float)]
distanceMatrixHelp h list | list == [] = []
                          | otherwise = (name h, name (head list), distance h (head list)) : distanceMatrixHelp (head list) (tail list)
