slice :: [a] -> Int -> Int -> [a]
slice l m n = map (\x -> l!!x) [x | x <- [m-1..n-1]]