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

readTree :: [Char] -> HTree
readTree str = snd (rebuildTree str)

-- build the huffman tree from the serialized string
rebuildTree :: [Char] -> ([Char],HTree)
rebuildTree ('1':c:xs) = (xs, Leaf 0 c)
rebuildTree ('0':xs) = 
	let
		ndLeft 	= (rebuildTree xs)
		newStr 	= fst(ndLeft)
		ndRight = (rebuildTree newStr)
		finalStr =  (fst ndRight)
		newNode  = Node 0 (snd ndLeft) (snd ndRight)
	in (finalStr, newNode)

-- return the decoded string from the encoded text using passed the huffman tree
decode :: HTree -> [Char] -> String
decode wholeTrie path = decodeByt wholeTrie path where
	decodeByt (Node _ t1 _) ('1':rest) = decodeByt t1 rest
	decodeByt (Node _ _ t2) ('0':rest) = decodeByt t2 rest
	decodeByt (Leaf _ c) rest = c : (decodeByt wholeTrie rest)
	decodeByt _ [] = []

main :: IO()
main = do 
			coded <- readFile "./fichero.huff"
			t <- readFile "./fichero.tree"
			let hTree = readTree t
			writeFile "./fichero.dec" (decode hTree coded)


