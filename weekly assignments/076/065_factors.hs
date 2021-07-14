import Data.List

ehPrimo x = length [y | y <- [1..x], x `mod` y == 0] <= 2
factors' num = [fac | fac <- [2..num-1], mod num fac == 0, ehPrimo fac, _ <- [fac | i <- [1..num], num `mod` fac^i == 0] ]
factors num = zip noDuplicates powers
    where
        allFactors = factors' num
        howMany x = length $ filter ( == x) allFactors
        noDuplicates = nub allFactors
        powers = [howMany x | x <- noDuplicates]