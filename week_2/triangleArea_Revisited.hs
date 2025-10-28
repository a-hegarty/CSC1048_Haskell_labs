triangleArea_revisited :: Float -> Float -> Float -> Float
triangleArea_revisited a b c = sqrt(s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c)/2