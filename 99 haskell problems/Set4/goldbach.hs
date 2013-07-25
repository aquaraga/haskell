goldbach :: (Integral a) => a -> (a, a)
goldbach n
    | or [n <=2, odd n] = error "Goldbach conjecture talks about only even numbers greater than 2"
    | True = getPrimes 2 (n - 2) where
         getPrimes x y
           | and [isPrime x, isPrime y] = (x, y)
           | otherwise = getPrimes (succ x) (pred y)

isPrime :: (Integral a) => a -> Bool
isPrime n
    | n >= 2 =  null $ filter (== 0) $ map (mod n) [2..(quot n 2)]
    | otherwise = False