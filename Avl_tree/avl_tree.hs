import Text.PrettyPrint

-- AVL trees take the form Node Root Left right
-- tree1/2\3\4 will take the form Node 2 (Node 1 empty empty) (Node 3 empty (Node 4 empty empty))
data AVL_Tree t  = Empty | Node t (AVL_Tree t) (AVL_Tree t)
    deriving(Show, Eq)

prettyTree :: Show t => AVL_Tree t -> Doc
prettyTree Empty = text "Empty"
prettyTree (Node t l r) = text (show t)
    $+$ nest 1 (prettyTree l)
    $+$ nest 1 (prettyTree r)

