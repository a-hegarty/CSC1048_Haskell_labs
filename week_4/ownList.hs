myAppend :: [a] -> [a] -> [a]
myAppend x [] = x
myAppend [] y = y
myAppend x y = x ++ y

myHead :: [a] -> a
myHead (x:xs) = x

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]
myTail (x:xs) = xs

myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = x : myInit xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs


mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct (x:xs) = x * mySum xs

myMaximum :: Ord a => [a] -> a
myMaximum [x] = x
myMaximum (x:xs)  = if x > myMaximum xs 
                    then x 
                    else myMaximum xs    


myMinimum :: Ord a => [a] -> a
myMinimum [x] = x
myMinimum (x:xs)  = if x < myMinimum xs 
                    then x 
                    else myMinimum xs 

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
    | a == x = True
    |otherwise = myElem a xs

myDelete :: Eq a => a -> [a] -> [a]
myDelete a [] = []
myDelete a (x:xs) = if a == x 
                    then xs
                    else x : myDelete a xs