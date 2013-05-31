import Data.List (group)

encodeModified :: (Ord a, Show a) => [a] -> [String]
encodeModified l = map ordinal (group l)
       where ordinal (x:[]) = "Single " ++ show x
             ordinal xs = "Multiple " ++ show (length xs) ++ " " ++ show (head xs)