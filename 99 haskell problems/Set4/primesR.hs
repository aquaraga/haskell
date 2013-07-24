primesR :: (Integral a) => a -> a -> [a]
primesR m n
    | m > n = []
    | isPrime m = m:primesR (succ m) n
    | otherwise = primesR (succ m) n


isPrime :: (Integral a) => a -> Bool
isPrime n
    | n >= 2 =  null $ filter (== 0) $ map (mod n) [2..(quot n 2)]
    | otherwise = False