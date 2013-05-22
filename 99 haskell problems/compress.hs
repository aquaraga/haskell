import Data.List (group)

compress :: (Ord a) => [a] -> [a]
compress l = comprss $ group l
      where 
	        comprss [] = []
	        comprss (x:xs) = (head x): comprss xs

			
compress' :: (Ord a) => [a] -> [a]
compress' (x:xs@(y:_))
        | x == y = compress' xs
        | otherwise = x : compress' xs
compress' (x:[]) = [x]
		