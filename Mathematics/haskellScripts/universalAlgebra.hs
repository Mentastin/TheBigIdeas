import Data.Tree
import Data.List
import Data.Maybe
import qualified Data.Set
import Data.Set (Set, toList, fromList, elemAt, unions, empty)

equivalenceClass :: (a -> a -> Bool) -> Set a -> a -> Set a
equivalenceClass equivRel set elem = Data.Set.filter (equivRel elem) set

quotientSet :: Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)
quotientSet equivRel set = Data.Set.map (equivalenceClass equivRel set) set


setToForest :: (Ord a) => Set [a] -> [Tree a]
setToForest set = unfoldForest nodeGenerator subTrees where
    subTrees = toList $ quotientSet (\x y -> head x == head y) (Data.Set.delete [] set)
    nodeGenerator subTreeSet = (head $ elemAt 0 subTreeSet, subSubTrees) where
        subSet = Data.Set.delete [] $ Data.Set.map tail subTreeSet
        subSubTrees = toList $ quotientSet (\x y -> head x == head y) subSet


forestToSet :: (Ord a) => [Tree a] -> Set [a]
forestToSet forest = unions $ map treeToSet forest where
    treeToSet tree = Data.Set.union (Data.Set.map ((rootLabel tree):) (forestToSet $ subForest tree)) (fromList [[rootLabel tree]])

-- TODO with Arrows??
prodMap :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
prodMap f g (x,y) = (f x, g y)

data Token = Var {token :: String} | Op {token :: String, arity :: Int}
    deriving (Show, Eq, Ord)

isTerm :: [Token] -> Bool
isTerm [] = False
isTerm (Var v:tokens) = length tokens == 0
isTerm (Op f ar:tokens) = case (splitTerms tokens) of
    Nothing -> False
    Just termList -> length termList == ar

splitTerms :: [Token] -> Maybe [[Token]]
splitTerms [] = Just []
splitTerms tokens = case (find isTerm (inits tokens)) of
    Nothing -> Nothing
    Just firstTerm -> case (splitTerms (drop (length firstTerm) tokens)) of
        Nothing -> Nothing
        Just rest -> Just (firstTerm:rest)

{-
syntaxTree :: [Token] -> Set [String]
syntaxTree [] = empty
syntaxTree (Var v:_) = fromList [["Var"]]
syntaxTree (Op op ar:tokens) = Data.Set.union zeroListSingleton restOfTree where
    zeroListSingleton = fromList [["0p"]]
    restOfTree = unions $ map subSyntaxTree (zip [1..] (fromJust $ splitTerms tokens)) where
        subSyntaxTree (i, tokenList) = Data.Set.map ((show i):) (syntaxTree tokenList)
-}

syntaxTree :: [Token] -> Set [String]
syntaxTree [] = empty
syntaxTree (Var v:_) = fromList [[], ["(Var)"]]
syntaxTree (Op op ar:tokens) = Data.Set.union zeroListSingleton restOfTree where
    zeroListSingleton = fromList [[], ["(Op)"]]
    restOfTree = unions $ [fromList [(show i):p | p <- toList (syntaxTree ((fromJust $ splitTerms tokens) !! i))] | i <- [0..ar-1]]

{-
TEST:  putStr $ drawForest $ setToForest $ syntaxTree t
t1 = [Op "f" 2, Op "g" 2, Var "x", Var "y", Var "z"]
t2 = [Op "f" 2, Var "x", Op "g" 2, Var "y", Var "z"]
-}

lexer :: String -> [Token]
lexer str = map charToToken str where
    charToToken c
        | elem c "abc" = Op [c] 0
        | elem c "defg" = Op [c] 1
        | elem c "hijklmn" = Op [c] 2
        | elem c "opqrst" = Op [c] 3
        | elem c "uvwxyz" = Var [c]

-- Quotient list
groupAllBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAllBy f list = groupAllByHelper f list [] where
    groupAllByHelper :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
    groupAllByHelper f [] groups = groups
    groupAllByHelper f (l:ls) groups = groupAllByHelper f rest ((l:nextGroup):groups) where
        (nextGroup, rest) = partition (f l) ls