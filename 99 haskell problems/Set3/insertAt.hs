insertAt :: a -> [a] -> Int -> [a]
insertAt x l n = take (n - 1) l ++ x:(drop (n - 1) l)