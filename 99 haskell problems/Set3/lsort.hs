lsort :: [[a]] -> [[a]]
lsort [] = []
lsort [[]] = [[]]
lsort (x:xs) = let less = [y | y <- (filter (\ z -> length z < length x) xs)]
                   more = [y | y <- filter (\ z -> length z > length x) xs]  in  lsort(less) ++ [x] ++ lsort(more)