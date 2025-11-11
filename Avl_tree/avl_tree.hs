import Text.PrettyPrint

data AVL_Tree t  = Empty | Node t (AVL_Tree t) (AVL_Tree t)
    deriving(Show, Eq)

prettyTree :: Show t => AVL_Tree t -> Doc
prettyTree Empty = text "Empty"
prettyTree (Node t l r) = text (show t)
    $+$ nest 1 (prettyTree l)
    $+$ nest 1 (prettyTree r)

