type Poly = [Int]

evalPoly :: Poly -> Int -> Int
evalPoly [x] _ = x
evalPoly (x: xs) a = x + (a * (evalPoly xs a))
