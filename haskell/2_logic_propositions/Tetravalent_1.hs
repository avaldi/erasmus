module Tetravalent_1 (Formula, symbols, truthTable, checkTrue) where

import Data.List
import Control.Monad

data Formula = Fx Int
	| Formula :-> Formula
	deriving Read

instance Show Formula where
	show(Fx n) = "x" ++ show n
	show(p :-> q) = "(" ++ show p ++ " -> " ++ show q ++ ")"

symbols :: Formula -> [Int]
symbols (Fx a) = [a]
symbols (Fx a :-> Fx b) = [a,b]
symbols (a :-> b) = nub((symbols a) ++ (symbols b))

nSymbols :: Formula -> Int
nSymbols f = length (symbols f)

ev :: Formula -> [(Int, Int)] -> Int
ev (Fx a) val = snd ((filter (\x -> fst(x) == a) val)!!0)
ev (a :-> b) v = 
	let
		aEv = (ev a v) 
		bEv = (ev b v)
	 in 
		case (aEv, bEv) of
			(1,_) -> bEv

			(2, 1) -> 1
			(2, 2) -> 1
			(2, 3) -> 3
			(2, 4) -> 3

			(3, 1) -> 1
			(3, 2) -> 2
			(3, 3) -> 1
			(3, 4) -> 2

			(4, 1) -> 1
			(4, 2) -> 2
			(4, 3) -> 1
			(4, 4) -> 1

truthTable :: Formula -> IO()
truthTable f = do putStrLn ("\nValue of x" ++ show (symbols f) ++ " => Result\n")
                  mapM_ putStrLn [toStr a ++ " => " ++ show (ev f (zip (symbols f) a)) | a <- args (nSymbols f)]
                     where args n = replicateM n [1..4]
                           toStr = concat . intersperse " " . map show


tTableList :: Formula -> [[Int]]
tTableList f = [a ++ [(ev f (zip (symbols f) a))] | a <- args (nSymbols f)]
                     where args n = replicateM n [1..4]

checkTrue :: Formula -> Bool
checkTrue f =
	let 
		lastV = map last (tTableList f)
	in
		all (==1) lastV

