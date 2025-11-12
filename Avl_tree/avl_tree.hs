import Text.PrettyPrint

-- AVL trees take the form Node Root Left right
-- tree1/2\3\4 will take the form Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))
data AVL_Tree t  = Empty | Node t (AVL_Tree t) (AVL_Tree t)
    deriving(Show, Eq, Ord)

print_Tree :: Show t => AVL_Tree t -> Doc
print_Tree Empty = text "Empty"
print_Tree (Node t l r) = text (show t)
    $+$ nest 1 (print_Tree l)
    $+$ nest 1 (print_Tree r)

