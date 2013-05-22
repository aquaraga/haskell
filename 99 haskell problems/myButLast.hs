myButLast :: [a] -> a
myButLast [] = error "list cannot be empty"
myButLast (x:[]) = error "list should have atleast 2 elements"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs