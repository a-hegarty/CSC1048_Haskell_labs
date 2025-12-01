import Text.PrettyPrint

-- AVL trees take the form Node Root Left right
-- tree 1/2\3\4 will take the form; (Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty Empty)))
data AVL_Tree t  = Empty | Node t (AVL_Tree t) (AVL_Tree t)
    deriving(Show, Eq, Ord)

-- when calling, avl tree MUST be in brackets
-- e.g. print_Tree (Node 2 empty empty 0)
print_Tree :: Show t => AVL_Tree t -> Doc
print_Tree Empty = text "Empty"
print_Tree (Node t l r) = text (show t)
    $+$ nest 1 (print_Tree l)
    $+$ nest 1 (print_Tree r)

--function outputs the height of a given avl tree
height :: AVL_Tree t -> Int
height Empty = 0
height (Node _ Empty Empty) = 0
height (Node t l r) = 1 + max(height l) (height r)

--function inserts a node into the avl tree using correct balancing
insert_Node :: (Ord n, Num n) => n -> AVL_Tree n -> AVL_Tree n
insert_Node n Empty = (Node n Empty Empty)
insert_Node n (Node t left right)
    | n > t = rotate((Node t left (insert_Node n right)))
    | otherwise = rotate((Node t (insert_Node n left) right))

--function returns a bool of whether the AVL tree is balanced or not
is_Balanaced :: (Ord n, Num n) => AVL_Tree n -> Bool
is_Balanaced Empty = True
is_Balanaced (Node t l r)
    |not (is_Balanaced l) = False
    |not (is_Balanaced r) = False
    |abs ((height l) - (height r)) > 1 = False
    |otherwise = True

left_Tree :: AVL_Tree t -> AVL_Tree t
left_Tree Empty = Empty
left_Tree (Node _ l _) = l

right_Tree :: AVL_Tree t -> AVL_Tree t
right_Tree Empty = Empty
right_Tree (Node _ _ r) = r

root :: (Ord n, Num n) => AVL_Tree n -> n
root Empty = 0
root (Node t l r) = t

rotate :: (Ord a, Num a) => AVL_Tree a -> AVL_Tree a
rotate Empty = Empty
rotate (Node t l r)
    | not (is_Balanaced r) = (Node t l (rotate r)) 
    | (height l) + 1 < (height r) && (height (left_Tree r)) < (height (right_Tree r)) =
        (Node (root r) (Node t l (left_Tree r)) (right_Tree r)) -- right-right rotation
    | (height r) + 1 < (height l) && (height (left_Tree l)) < (height (right_Tree l)) =
        (Node (root l) (left_Tree l) (Node t (right_Tree l) r)) -- left-left rotation
    | (height l) + 1 < (height r) && (height (left_Tree r)) > (height (right_Tree r)) =
        (Node (root (left_Tree r)) (Node t l (left_Tree(left_Tree r))) (Node (root r) (right_Tree(left_Tree r)) (right_Tree r))) -- right-left rotation
    | (height r) + 1 < (height l) && (height (right_Tree l)) > (height (left_Tree l)) =
        (Node (root(right_Tree l)) (Node (root l) (left_Tree l) (left_Tree(right_Tree l))) (Node t (right_Tree(right_Tree l)) r)) -- left-right rotation
    | otherwise = (Node t l r) -- return original tree if no rotations apply