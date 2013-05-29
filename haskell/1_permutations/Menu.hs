import System.IO
import Data.List
import Permutation

main::IO()
main = do 
		opt <- menu
		case opt of
			'c' -> do 
					mnuCompose
					main
			'i' -> do
					mnuInverse
					main
			'd' -> do 
					mnuCycles
					main
			'q' -> putStrLn "Quit..."

menu::IO Char
menu = do
		putStrLn "\n --- Example program to manage permutation data type ---"
		putStrLn " c - Compose 2 permutation"
		putStrLn " i - Get the inverse"
		putStrLn " d - Get the disjoint cycles decomposition"
		putStrLn " q - Quit"
		putStrLn "Choice: "
		choice <- getLine
		let x = head choice
		if elem x "cidq" then return x 
			else do
				putStrLn "The choice is not correct, try again..."
				menu

mnuCompose::IO()
mnuCompose = do
		putStrLn "\n * --- COMPOSITION --- *"
		putStrLn "\n * FIRST PERMUTATION *"
		perm1 <- getPerm
		putStrLn "\n * SECOND PERMUTATION *"
		perm2 <- getPerm
		if (permSize perm1) == (permSize perm2) then 
				do 
					putStrLn ("\nThe composition of\n" ++ (show perm1) ++ "\nand\n" ++ (show perm2) ++ "\nis")
					putStrLn ( show (perm1 <*> perm2) )
			else do
				putStrLn "*** I am sorry, the permutations cannot be composed - (Different size)"
				mnuCompose

mnuInverse::IO()
mnuInverse = do
		putStrLn "\n * --- INVERSE --- *"
		perm <- getPerm
		let inv = inverse perm
		putStrLn ("\nThe inverse of\n" ++ (show perm) ++ "\nis")
		putStrLn (show inv)

mnuCycles::IO()
mnuCycles = do
		putStrLn "\n * --- DISJOINT CYCLES --- *"
		perm <- getPerm
		let cycles = toCycles perm
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


