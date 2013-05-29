import Data.List
import Data.Ord (comparing)
import Data.Maybe
import System.Environment

data HTree = 
	Leaf Int Char 			-- weight, symbols
	| Node Int HTree HTree 	-- weight, left, right
	| NullTree
	deriving (Show)

-- return the node weight
weight :: HTree -> Int
weight (Leaf n _) = n
weight (Node n _ _) = n

type HTable = [(Char, [Char])]

-- count the frequency of each symbols in a string
freqCount :: String -> [(Char, Int)]
freqCount "" = []
freqCount s = 
	let
		gr = group (sort s)
		ch = map head gr
		fr = map length gr
		in zip ch fr
-- sort frequency table from lower frequency to highest
sortFreq :: [(Char, Int)] -> [(Char, Int)]
sortFreq []     = []
sortFreq (x:xs) = sortFreq (filter (\z -> snd(z) < snd(x)) xs) ++ [x] ++ sortFreq (filter (\z -> snd(z) >= snd(x)) xs)

-- look for the code associated to a symbol
lookupTable :: HTable -> Char -> [Char]
lookupTable [] c = error ("lookupTable: No " ++ [c])
lookupTable ((ch,n):tb) c
	| (ch==c) = n
	| otherwise = lookupTable tb c

-- build the huffman tree from a string
buildTree :: String -> HTree
buildTree str = 
	let 
		freq = freqCount str
		nodeList = toNodesList (sortFreq freq)
		in (linkNodes nodeList)
-- create a node for each (symbol, frequency) pair
toNodesList :: [(Char,Int)] -> [HTree]
toNodesList freqs = [Leaf n c | (c,n) <- freqs]

-- connect each node and return the root of the built huffman tree
linkNodes :: [HTree] -> HTree
linkNodes [x] = x
linkNodes (x0:x1:xs) = linkNodes (insertOrdTree (joinNodes x0 x1) xs)

-- insert a node into a nodes-array ordered by frequency
insertOrdTree :: HTree -> [HTree] -> [HTree]
insertOrdTree t [] = [t]
insertOrdTree t (s:ss)
	| (weight t <= weight s) = t:s:ss
	| otherwise = s : insertOrdTree t ss

-- join to nodes returning a new node with the 2 nodes as leaves and 
-- the sum of 2 weight as weight
joinNodes :: HTree -> HTree -> HTree
joinNodes t1 t2 = Node (weight t1 + weight t2) t1 t2           
                           
-- return the huffman code table                                                                              
codeTable :: HTree -> HTable
codeTable t = convert [] t

convert :: [Char] -> HTree -> HTable
convert path (Leaf _ c) = [(c, reverse path)]
convert path (Node _ t1 t2) = (convert ('1':path) t1) ++ (convert ('0':path) t2)

-- encode all the text using the table
encode :: HTable -> String -> String
encode tbl msg = concat (map (lookupTable tbl) msg)

-- return the string containing the huffman tree serialized to be stored
storeTree :: [Char] -> HTree -> [Char]
storeTree out (Leaf _ c) = out ++ ['1'] ++ [c]
storeTree out (Node _ t1 t2) = out ++ ['0'] ++ (storeTree out t1) ++ (storeTree out t2)

main :: IO()
main = do 
			s <- readFile "./fichero.ext"
			let hTree = buildTree s
			let table = codeTable (hTree)
			let coded = encode table s
			writeFile "./fichero.tree" (storeTree [] hTree)
			writeFile "./fichero.huff" coded


