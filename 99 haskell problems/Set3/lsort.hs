import Data.List
import Data.Ord
import qualified Data.Map as Map

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort [[]] = [[]]
lsort (x:xs) = let less = [y | y <- (filter (\ z -> length z < length x) xs)]
                   more = [y | y <- filter (\ z -> length z > length x) xs]  in  lsort(less) ++ [x] ++ lsort(more)
				   


qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = let less = [y | y <- filter (<x) xs]
                   more = [y | y <- filter (>x) xs] in qsort(less) ++ [x] ++ qsort(more)

lfsort :: [[a]] -> [[a]]
lfsort xs = foldl (\acc x -> acc ++ [y | y <- filter (\z -> length z == fst x) xs]) [] $ sortBy (comparing $ snd) $ map (\x -> (head x, length x)) $ group $ sort $ map length xs


lfsort' :: [[a]] -> [[a]]
lfsort' xs = foldl (\acc x -> acc ++ snd x) [] $ sortBy compareListLengths $ Map.toList $ Map.fromListWith (++)  $ map (\x -> (length x, [x])) xs

compareListLengths :: (Int, [[a]]) -> (Int, [[a]]) -> Ordering
compareListLengths e1 e2 = compare (length $ snd e1) (length $ snd e2)