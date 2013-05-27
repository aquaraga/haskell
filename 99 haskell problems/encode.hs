import Data.List (group)

encode :: (Ord a) => [a] -> [(Int, a)]
encode l = map ordinal (group l)
       where ordinal subl = (length subl, head subl)