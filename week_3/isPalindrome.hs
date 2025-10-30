isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = if a == reverse a
                then True
                else False