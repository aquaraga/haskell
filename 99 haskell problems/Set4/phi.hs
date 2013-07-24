import Data.List

phi :: (Integral a) => a -> a
phi n = foldl (*) 1  $ map (\t ->((pred $ fst t) * (fst t) ^  (pred $ snd t))) $ primeFactorsMult n

primeFactorsMult n = map (\e -> (head e, length e)) $ group $ primeFactors n

primeFactors n = addFactors n 2
    where addFactors num prime
              | prime * prime > num = [num]
              | mod num prime == 0 = prime:(addFactors (quot num prime) prime)
              | otherwise = addFactors num (succ prime)