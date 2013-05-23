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