table :: (Bool -> Bool -> Bool) -> IO()
table f = mapM_ putStrLn [foldl (++) "" [show a, " ", show b, " --> ", show (f a b)] | a <- [False, True], b <- [False, True]]


and' :: Bool -> Bool -> Bool
and' a b = a && b

or' :: Bool -> Bool -> Bool
or' a b = a || b

not' :: Bool -> Bool
not' True = False
not' False = True

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

impl' :: Bool -> Bool -> Bool
impl' a b = (not' a) `or'` b

equ' :: Bool -> Bool -> Bool
equ' a b = not' $ xor' a b