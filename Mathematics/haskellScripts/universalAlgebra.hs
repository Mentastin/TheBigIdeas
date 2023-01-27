import Data.Tree
import Data.List
import qualified Data.Set
import Data.Set (Set, toList, fromList, elemAt, unions)

equivalenceClass :: (a -> a -> Bool) -> Set a -> a -> Set a
equivalenceClass equivRel set elem = Data.Set.filter (equivRel elem) set

quotientSet :: Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)
quotientSet equivRel set = Data.Set.map (equivalenceClass equivRel set) set


setToForest :: (Ord a) => Set [a] -> [Tree a]
setToForest set = unfoldForest nodeGenerator subTrees where
    subTrees = toList $ quotientSet (\x y -> head x == head y) set
    nodeGenerator subTreeSet = (head $ elemAt 0 subTreeSet, subSubTrees) where
        subSet = Data.Set.delete [] $ Data.Set.map tail subTreeSet
        subSubTrees = toList $ quotientSet (\x y -> head x == head y) subSet


forestToSet :: (Ord a) => [Tree a] -> Set [a]
forestToSet forest = unions $ map treeToSet forest where
    treeToSet tree = Data.Set.union (Data.Set.map ((rootLabel tree):) (forestToSet $ subForest tree)) (fromList [[rootLabel tree]])

data Token = Terminal {token :: String} | Nonterminal {token :: String, arity :: Int} 

syntaxTree :: [Token] -> Set [String]
syntaxTree [] = fromList [[]]
syntaxTree (Terminal term:_) = fromList [[]]
syntaxTree (Nonterminal op ar:tokens) = unions [Data.Set.map ((show i):) (syntaxTree (drop i tokens)) | i <- [0..ar] ]

{-
TEST:  putStr $ drawForest $ setToForest $ syntaxTree t
t1 = [Nonterminal "f" 2, Nonterminal "g" 2, Terminal "x", Terminal "y", Terminal "z"]
t2 = [Nonterminal "f" 2, Terminal "x", Nonterminal "g" 2, Terminal "y", Terminal "z"]
-}

-- Quotient list
groupAllBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAllBy f list = groupAllByHelper f list [] where
    groupAllByHelper :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
    groupAllByHelper f [] groups = groups
    groupAllByHelper f (l:ls) groups = groupAllByHelper f rest ((l:nextGroup):groups) where
        (nextGroup, rest) = partition (f l) ls