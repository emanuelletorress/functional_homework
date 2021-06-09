iguais3 x y z
    | x == y && y == z = 3
    | x == y || y == z || z == x = 2
    | otherwise = 0