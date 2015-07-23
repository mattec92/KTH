import Data.Char 

-- De två basfallen och det rekursiva fallet där man lägger ihop de två senaste.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

isConst c = c `elem` ['b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','z']
-- behövs inte isVowel c = c `elem` ['a','e','i','o','u','y']

-- Om tom, returnera tom.
-- Om första bokstaven k är konstant, returnera kok+rovarsprak på svansen.
-- Annars returnera bokstaven, underförstått vokal.
rovarsprak :: String -> String
rovarsprak s 
	| s == [] = []
	| isConst (head s) = (head s):'o':(head s):rovarsprak (tail s)
	| otherwise = (head s):rovarsprak (tail s)

-- Om tom, returnera tom.
-- Om första bokstaven konsonant, returnera första bokstaven och anropa svenska på svans-svans-svans,
-- dvs på allt utom de tre första bokstäverna.
-- Annars returnera huvudet och anropa svenska på svansen.
svenska :: String -> String
svenska s 
	| s == [] = []
	| isConst (head s) = (head s):svenska (tail (tail (tail s)))
	| otherwise = (head s):svenska (tail s)

-- Om någon lista är tom, returnera den andra.
-- Annars jämför första talet i listan och returnera det största och anropa merge på svansen på rätt lista
-- med den andra orörd.
merge [] b = b
merge a [] = a
merge a b = if (head a) >= (head b)
	then (head a):(merge (tail a) b)
	else (head b):(merge a (tail b))


-- Om s är tom finns det inga fler ord och vi returnerar det största värdet, current kommer innehålla det
-- senaste ordet vi läst in så vi anropar max på den tidigare största och current.
-- Om huvudet är en bokstav anropar vi maxcalc igen och lägger till 1 på current-räknaren
-- Om det inte är en bokstav så är ordet slut och vi jämför current med den förra längsta och anropar 
-- maxcalc på svansen med antingen current eller det förra längsta som längsta och sätter current till 0.
maxord :: String -> Int
maxord s = maxcalc s 0 0
maxcalc s longest current
	| s == [] = max longest current
	| isAlpha (head s) = maxcalc (tail s) longest (current+1)
	| current > longest = maxcalc (tail s) current 0
	| otherwise = maxcalc (tail s) longest 0


-- Varje beräkning av nästa fib-tal sparas i anropet. Anropet kommer köras tills n räknat ner till basfallen.
-- För varje körning sparar vi det nya ihopräknade talet i första parametern och det förra i andra.
ackfib :: Int -> Int
ackfib n = fibcalc n 1 0
fibcalc 0 a b = 0
fibcalc 1 a b = a
fibcalc n a b = fibcalc (n-1) (a+b) a
