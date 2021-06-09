max3 x y z
    | x >= y && y >= z || x >= z && z >= y = x
    | y >= x && x >= z || y >= z && z >= x = y
    | otherwise = z