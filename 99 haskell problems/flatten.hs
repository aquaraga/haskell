-- Okay I cheated. I saw the solution!

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten x = let
    flatten' acc (Elem e) = e:acc
    flatten' acc (List (x:xs)) = flatten' (flatten' acc (List xs)) x
    flatten' acc (List []) = acc
            in flatten' [] x