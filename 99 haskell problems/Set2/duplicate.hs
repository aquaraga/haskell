duplicate :: [a] -> [a]
duplicate = foldr (\x acc -> x:x:acc) []