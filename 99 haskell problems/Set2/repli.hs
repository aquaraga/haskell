repli :: [a] -> Int -> [a]
repli l n = foldl (\acc x -> acc ++ (replicate n x)) [] l 