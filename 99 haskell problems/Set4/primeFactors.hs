import Data.List



primeFactors n = addFactors n 2
    where addFactors num prime
              | prime * prime > num = [num]
              | mod num prime == 0 = prime:(addFactors (quot num prime) prime)
              | otherwise = addFactors num (succ prime)


primeFactors' n = addFactors n []
   where addFactors num arr
             | num == 1 = arr
             | otherwise = let factor = findFirstFactor num in addFactors (quot num factor) (factor:arr)


--Greedily finds the first factor
findFirstFactor :: (Integral a) => a -> a
findFirstFactor n = case find (\x -> n `mod` x == 0) (reverse $ primeNumbersTill n) of
    Nothing -> 1
    Just x  -> x

primeNumbersTill :: (Integral a) => a -> [a]
primeNumbersTill n = filter isPrime [2..n]

isPrime :: (Integral a) => a -> Bool
isPrime n
    | n >= 2 =  null $ filter (== 0) $ map (mod n) [2..(quot n 2)]
    | otherwise = False