import System.IO
import Data.List
import Proposition -- (symbols, truthTable, cnf)

main::IO()
main = do 
		putStrLn "\n --- Example program to manage proposition data type ---"
		putStrLn "- s -> start"
		putStrLn "- q -> quit"
		
		choice <- getLine
		case choice of 
			"s" -> getProp
			"q" -> putStrLn "Quit..."
			_ -> do 
					putStrLn "Your choice is not correct..."
					main

				
getProp :: IO()
getProp = do
			putStrLn "\nPlease write a logical proposition using"
			putStrLn "the constructor (Fx n) to define the n-variable"
			putStrLn "and the following boolean operators: :->, :<->, :/\\, :\\/"
			putStrLn "as logical operators"
			putStrLn "\n- q -> quit"
			
			ps <- getLine
			if (ps=="q") then
				putStrLn "Quit..."
				else
					do 
						let prop = read ps::Formula
						putStrLn ("Ok, the proposition is " ++ (show prop))
						mnuProp prop
						getProp						
{-
	let prop = catch readProp (\err -> putStrLn ("Error: " ++ (show err)))
	where readProp s = do 
-}							
							
					
mnuProp :: Formula -> IO()
mnuProp prop = do
				putStrLn "\n- Functions availables: symbols, truthTable, cnf"
				putStrLn "- new -> work with a new proposition"
	
				cmd <- getLine
				case cmd of
					"symbols" -> do 
									putStrLn (show (symbols prop))
									mnuProp prop
					"truthTable" -> do 
										(truthTable prop)
										mnuProp prop
					"cnf" -> do
								(cnf prop)
								mnuProp prop
					"new" -> putStrLn "......"
				
