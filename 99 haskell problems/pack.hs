pack :: (Ord a) => [a] -> [[a]]
pack x = let
        pack' acc [] = acc
        pack' [] (y:ys) = pack' [[y]] ys
        pack' acc (y:ys)
              | y == (head (last acc)) = pack' (init acc ++ [y:(last acc)]) ys
              | otherwise = pack' (acc ++ [[y]]) ys
     in pack' [] x  