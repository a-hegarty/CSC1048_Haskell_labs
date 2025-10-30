data BinTree t = Empty | Root t (BinTree t) (BinTree t)
            deriving (Eq, Ord, Show)

leaf x = Root x Empty Empty

addNode :: Ord a => a -> BinTree a -> BibTree a