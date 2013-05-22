myLast :: [a] -> a
myLast [] = error "List is empty"
myLast ([x]) =  x
myLast (x:xs) = myLast xs


myLast' :: [a] -> a
myLast' x = case x of [] -> error "List is empty"
                      [s] -> s
                      (x:xs) -> myLast' xs
					  
myLast'' :: [a] -> a
myLast'' xs = head (reverse xs)