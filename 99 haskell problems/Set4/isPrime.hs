import Data.List

isPrime :: Int -> Bool
isPrime n
    | n > 2 =  null $ filter (== 0) $ map (mod n) [2..(quot n 2)]
    | otherwise = False