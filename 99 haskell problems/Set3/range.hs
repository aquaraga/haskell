range :: Int -> Int -> [Int]
range low high = [low..high]

range' :: Int -> Int -> [Int]
range' low high
    | low == high = [high]
    | otherwise = low:(range' (low + 1) high)
