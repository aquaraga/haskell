import System.Random
import Data.List

int_permu :: Int -> [Int]
int_permu n = let gen = mkStdGen 10 in take n $ nub $ randomRs (1, n) gen

rnd_permu :: [a] -> [a]
rnd_permu l = map (\x -> l!! pred x) (int_permu (length l))

