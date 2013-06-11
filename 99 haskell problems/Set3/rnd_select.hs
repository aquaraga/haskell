import System.Random

rnd_select :: [a] -> Int -> [a]
rnd_select l n = map (\ x -> l !! (mod x (length l)))   (take n $ randoms (mkStdGen 10) :: [Int])