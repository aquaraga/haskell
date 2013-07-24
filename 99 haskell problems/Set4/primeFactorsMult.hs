import Data.List

primeFactorsMult n = map (\e -> (head e, length e)) $ group $ primeFactors n

primeFactors n = addFactors n 2
    where addFactors num prime
              | prime * prime > num = [num]
              | mod num prime == 0 = prime:(addFactors (quot num prime) prime)
              | otherwise = addFactors num (succ prime)