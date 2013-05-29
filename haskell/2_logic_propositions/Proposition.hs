module Proposition (Formula, symbols, truthTable, cnf) where

import Data.List
import Control.Monad

-- OR \/ +, AND /\ *

data Formula = Fx Int
	| Formula :-> Formula
	| Formula :<-> Formula
	| Formula :/\ Formula
	| Formula :\/ Formula
	| N Formula
	deriving Read

instance Show Formula where
	show(Fx n) = "x" ++ show n
	show(p :-> q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
	show(p :<-> q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"
	show(p :/\ q) = "(" ++ show p ++ " /\\ " ++ show q ++ ")"
	show(p :\/ q) = "(" ++ show p ++ " \\/ " ++ show q ++ ")"
	show(N p) = "N" ++ show p

{-
readProp :: String -> Either String TPermutation
readProp txt = 	do
-}

symbols :: Formula -> [Int]
symbols (Fx a) = [a]
symbols (Fx a :-> Fx b) = [a,b]
symbols (Fx a :<-> Fx b) = [a,b]
symbols (Fx a :/\ Fx b) = [a,b]
symbols (Fx a :\/ Fx b) = [a,b]
symbols (N a) = nub(symbols a)
symbols (a :-> b) = nub((symbols a) ++ (symbols b))
symbols (a :<-> b) = nub((symbols a) ++ (symbols b))
symbols (a :/\ b) = nub((symbols a) ++ (symbols b))
symbols (a :\/ b) = nub((symbols a) ++ (symbols b))

nSymbols :: Formula -> Int
nSymbols f = length (symbols f)

ev :: Formula -> [(Int, Bool)] -> Bool
ev (Fx a) val = snd ((filter (\x -> fst(x) == a) val)!!0)
ev (N a) v = not (ev a v)
ev (a :-> b) v = (not (ev a v)) || (ev b v)
--ev (a :<-> b) v = ( ev (a :-> b) v ) || ( ev (b :-> a) v )
ev (a :<-> b) v = ((ev a v) && (ev b v)) || (not (ev a v) && not (ev b v))
ev (a :/\ b) v = (ev a v) && (ev b v)
ev (a :\/ b) v = (ev a v) || (ev b v)

truthTable :: Formula -> IO()
truthTable f = do putStrLn ("\nValue of x" ++ show (symbols f) ++ " => Result\n")
                  mapM_ putStrLn [toStr a ++ " => " ++ show (ev f (zip (symbols f) a)) | a <- args (nSymbols f)]
                     where args n = replicateM n [False, True]
                           toStr = concat . intersperse " " . map show


tTableList :: Formula -> [[Bool]]
tTableList f = [a ++ [(ev f (zip (symbols f) a))] | a <- args (nSymbols f)]
                     where args n = replicateM n [False, True]

cnf :: Formula -> IO()
cnf f = do 
			let table = tTableList f
			let maxTerms = filter(\x -> (last x) == False) table
			let sym = symbols f
			let sumMaxTerms = map ((sumTerms sym).init) maxTerms
			let prod = multiplyFormulas sumMaxTerms
			putStrLn (show prod)
			

sumTerms :: [Int] -> [Bool] -> Formula
sumTerms sym mt = 
	if (length (tail sym) > 0) then
		if (head mt) == True then
			N(Fx (head sym)) :\/ sumTerms (tail sym) (tail mt)
		else
			(Fx (head sym)) :\/ sumTerms (tail sym) (tail mt)
	else
		if (head mt) == True then
			N(Fx (head sym))
		else
			Fx (head sym)

multiplyFormulas :: [Formula] -> Formula
multiplyFormulas fs = 
	if (length (tail fs) > 0) then
		(head fs) :/\ multiplyFormulas (tail fs)
	else
		(head fs)
