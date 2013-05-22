elementAt :: [a] -> Int -> a
elementAt x n
    | n > length x = error "List is too short"
    | n == 1 = head x
	| otherwise =  elementAt (tail x) (n - 1)