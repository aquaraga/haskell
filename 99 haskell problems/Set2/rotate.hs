rotate :: [a] -> Int -> [a]
rotate l n = let 
       indexList l n
          | n > 0 = [x | x <- [n..length l]] ++ [x | x <- [1..n-1]]
          | otherwise = [x | x <- [length l + n + 1..length l]] ++ [x | x <- [1..length l + n]]
	   in map (\x -> l!!x) (map pred (indexList l n))