import Data.List
import Data.Maybe

main = do 
		perm <- getPerm
		let inv = inverse perm
		let cycles = toCycles perm

		putStrLn ("\nThe inverse of\n" ++ (show perm) ++ "\nis")
		putStrLn (show inv)
		putStrLn ("\nThe disjoint cycles of\n" ++ (show perm) ++ "\nare")
		putStrLn (show cycles)

getPerm :: IO(TPermutation)
getPerm = do
		putStrLn "\nPlease write a permutation"
		
		permStr <- getLine
		let result = readPerm permStr
		case result of
		    Left msg -> do 
				putStrLn(msg)
				getPerm
		    Right perm -> return(perm)

data TPermutation =
	P [Int] -- list of Int, terms of the permutation


-- quando le 2 liste sono uguali, le permutazioni sono uguali
instance Eq TPermutation where
	P l1 == P l2 = l1==l2

{-
per mostrare la permutazione, mostro tutti i numeri 
da 1 al numero di elementi nella lista,	e sotto la lista
-}
instance Show TPermutation where
	show (P xs) = "(" ++ (mostrar[1..(length xs)]) ++ ")" ++ "\n" 
			   ++ "(" ++ (mostrar xs) ++ ")"

readPerm :: String -> Either String TPermutation
readPerm txt = 	do 
	let found = listToMaybe((readList txt))
	if (found == Nothing) then
		Left "*** Cannot read the permutation terms"
		else do 
				let list = fst(fromJust(found))
				if (nub list /= list) then Left "*** There are some duplicate terms"
					else if (sort list /= [1..length list]) then Left "*** Some permutation terms are missing"
						else Right (P(list))

mostrar :: [Int] -> String
mostrar[] = ""
mostrar(x:xs) = (show x) ++ " " ++ (mostrar xs)

{-
-creo una lista di coppie con numero d'ordine del termine nella
 permutazione.
-ordino le coppie.
-smonto le coppie
-}
inverse :: TPermutation -> TPermutation
inverse (P p) = 
			let
				zipped = (zip p [1..(length p)])
				sorted = sort(zipped)
				inverted = map snd sorted
			in
				P(inverted)
--
(<*>) :: TPermutation -> TPermutation -> TPermutation
(<*>) (P a) (P b) = P (composition a b)

composition :: [Int] -> [Int] -> [Int]
composition _ [] = []
composition [] _ = []
composition a (x:xs) =   a!!(x-1) : (composition a xs)

toCycles :: TPermutation -> [[Int]]
toCycles (P (x:xs)) =
	do
		let zipped  = (zip [1..(length (x:xs))] (x:xs))
		let csZipped = map (\y -> (getCycle y zipped)) [1..(length (x:xs))]
		let cs = map (\z -> (map snd z)) csZipped		
		nubBy (\x y -> sameCycles x y) (zipList cs [1..(length cs)])

getCycle :: Int -> [(Int,Int)] -> [(Int,Int)]
getCycle start list =  
	if (find (\x -> fst(x) == start) list) /= Nothing 
		then do let el = filter (\x -> fst(x) == start) list
			let nextStart = snd(el!!0)::Int
			if ((find (\x -> fst(x) == nextStart) list) == Nothing) 
				then []	
				else do	let newList = list \\ el
					(el!!0):(getCycle nextStart (newList))
	else []

{- Equality for a cycle -}
sameCycles :: [Int] -> [Int] -> Bool
sameCycles a b = ((sort a) == (sort b))

zipList :: [[a]] -> [a] -> [[a]]
zipList [] [] =  []
zipList (a:as) (b:bs) = (b:a) : (zipList as bs)

permSize :: TPermutation -> Int
permSize (P[]) = 0
permSize (P p) = length p
