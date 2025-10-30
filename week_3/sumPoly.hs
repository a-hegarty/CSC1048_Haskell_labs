type Poly = [Int]

sumPoly :: Poly -> Poly -> Poly
sumPoly [] [] = []
sumPoly x [] = x
sumPoly [] y = y
sumPoly (x:xs) (y:ys) = (x + y): sumPoly xs ys