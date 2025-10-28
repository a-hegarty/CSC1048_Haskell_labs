triangleArea2 :: Float -> Float -> Float -> Float
triangleArea2 a b c = sqrt(s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c)/2

isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c 
    | a + b > c = True
    | a + c > b = True
    | b + c > a = True
    | otherwise = False

triangleArea_revisited :: Float -> Float -> Float -> Float
triangleArea_revisited a b c =  
    if (isTriangle a b c)  
        then (triangleArea2 a b c)
    else error "Not a triangle"