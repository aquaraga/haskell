split :: [a] -> Int -> ([a], [a])
split l n = split' [] l n
      where split' l r 0 = (l, r)
            split' l r n = split' (l ++ [head r]) (tail r) (n - 1)