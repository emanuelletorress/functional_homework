-- 004
countNeg = length . filter (\x -> x < 0)

-- 005
final x xs = drop x' xs
    where
        size = length xs
        x' = size - x

-- 006
iguais3 x y z
    | x == y && y == z = 3
    | x == y || y == z || z == x = 2
    | otherwise = 0

-- 007
interior = tail . init

-- 008
gangorra p1 c1 p2 c2
    | pc1 == pc2 = 0
    | pc1 < pc2 = 1
    | otherwise = -1
    where
        pc1 = p1 * c1
        pc2 = p2 * c2

-- 010
min2 x y = if x < y then x else y
-- alternativa:
min2' x y = min x y

-- 011
min3 x y z
    | (x <= y && y <= z || x <= z && z <= y) = x
    | (y <= x && x <= z || y <= z && z <= x) = y
    | otherwise = z

-- 000
soma2 x y = x + y