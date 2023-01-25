import Data.Tree
import Data.Set

type SetTree a = Set[a] -- TODO: how to enforce that this is tree?

setToTree :: SetTree a -> Tree a
setToTree set = toAscList set

treeToSet :: Tree a -> SetTree a