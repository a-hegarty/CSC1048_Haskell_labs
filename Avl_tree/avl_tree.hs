import Text.PrettyPrint

-- AVL trees take the form Node Root Left right
-- tree1/2\3\4 will take the form Node 2 (Node 1 Empty Empty 1) (Node 3 Empty (Node 4 Empty Empty 2) 1) 0
data AVL_Tree t  = Empty | Node t (AVL_Tree t) (AVL_Tree t) Int
    deriving(Show, Eq, Ord)

-- when calling, avl tree MUST be in brackets
-- e.g. print_Tree (Node 2 empty empty 0)
print_Tree :: Show t => AVL_Tree t -> Doc
print_Tree Empty = text "Empty"
print_Tree (Node t l r _) = text (show t)
    $+$ nest 1 (print_Tree l)
    $+$ nest 1 (print_Tree r)

--function outputs the height of a given avl tree
height :: AVL_Tree t -> Int
height Empty = 0
height (Node _ Empty Empty) = 0
height (Node t l r h) = 1 + max(height l) (height r)

--function inserts a node into the avl tree using correct balancing
insert_Node :: (Ord n) => n -> AVL_Tree n -> AVL_Tree n
insert_Node n Empty = Node n Empty Empty 1
insert_Node n (Node t left right height)
    |n < t = (Node t (insert_Node n left) right (height + 1))
    |n > t = (Node t left (insert_Node n right) (height + 1))
    |otherwise = Node n left right 1