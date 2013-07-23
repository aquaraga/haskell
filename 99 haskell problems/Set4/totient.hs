totient :: Int -> Int
totient n
    | n < 1 = error "Function is defined only for positive numbers"
    | n == 1 = 1
    | otherwise = length $ filter (\x -> coprime x n) [1..pred n]

coprime a b = (gcd a b) == 1