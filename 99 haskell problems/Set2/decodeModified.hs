
decodeModified :: [String] -> String
decodeModified x = let
    decodeModified' s [] = s
    decodeModified' s (x:xs)
           | startsWith "Single" x = decodeModified' (s ++ [head (last (split ' ' x))]) xs
           | otherwise = decodeModified' (s ++ (replicate (read ((split ' ' x)!!1)::Int) (head (last (split ' ' x))))) xs
    in decodeModified' [] x

split delimiter = foldr f [[]] 
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

startsWith substr str =  take (length substr) str == substr
      