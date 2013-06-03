-- Examples

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
	
collatz :: (Integral a) => a -> [a]
collatz n
    | n == 1 = [1]
    | odd n = let num = 3*n + 1 in n:collatz num
    | otherwise = let num = (div n 2) in n:collatz num
	
longlists :: [a] -> Bool
longlists l = length l > 15
	
longchains :: (Integral a) => [[a]]
longchains = filter longlists $ map collatz [1..100]


pack :: (Ord a) => [a] -> [[a]]
pack x = let
        pack' acc [] = acc
        pack' [] (y:ys) = pack' [[y]] ys
        pack' acc (y:ys)
              | y == (head (last acc)) = pack' (init acc ++ [y:(last acc)]) ys
              | otherwise = pack' (acc ++ [[y]]) ys
     in pack' [] x  
	 
startsWith substr str =  take (length substr) str == substr



duplicate :: [a] -> [a]
duplicate = foldr (\x acc -> x:x:acc) []


duplicate [1..10]

dropEvery :: [a] -> Int -> [a]
dropEvery l n = map (\x -> l!!x) [x | x <- [1..length l - 1], mod x n /= 0]

slice :: [a] -> Int -> Int -> [a]
slice l m n = map (\x -> l!!x) [x | x <- [m-1..n-1]]