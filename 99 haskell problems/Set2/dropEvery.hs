dropEvery :: [a] -> Int -> [a]
dropEvery l n = map (\x -> l!!x) [x | x <- [1..length l - 1], mod x n /= 0]