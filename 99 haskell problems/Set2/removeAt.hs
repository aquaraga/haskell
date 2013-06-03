removeAt :: Int -> [a] -> (a, [a])
removeAt n l = (l !! (n - 1), map (\x -> l!!x) [x | x <- [0..length l - 1], x  /= (n - 1)])