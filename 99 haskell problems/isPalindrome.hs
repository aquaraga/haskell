isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome l = l == (reverse l)

isPalindrome' :: (Ord a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [x] = True
isPalindrome' (x:xs)
    | x == last xs = isPalindrome' (init xs)
	| otherwise = False