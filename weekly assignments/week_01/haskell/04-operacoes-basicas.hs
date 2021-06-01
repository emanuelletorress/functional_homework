-- 007
somaImpares = sum . filter (odd)

-- 001
max3 x y z
    | (x >= y && y >= z || x >= z && z >= y) = x
    | (y >= x && x >= z || y >= z && z >= x) = y
    | otherwise = z

-- 012
fatorial n
    | n == 0 = 1
    | otherwise = product reversedList
    where
        reversedList = reverse [1..n]

-- 014
elemento x xs = xs !! x'
    where
        x' = if x < 0 then x + tam else x
        tam = length xs

-- 014 recursiva
elemento' :: Int -> [a] -> a
elemento' 0 (x:xs) = x
elemento' n (x:xs) = elemento' (e-1) xs
    where
        e = if n < 0 then (n + tam) + 1 else n
        tam = length xs

-- 015
pertence x u = x `elem` u

-- 016 Caso 1
total xs = sum (map (\x -> 1) xs)

-- 016 Caso 2
total' [] = 0
total' (_:xs) = 1 + total' xs

-- 017


-- 023
corpo xs = init xs

-- 028

-- 030

-- 031

-- 047

-- 009
sublist first last xs = (drop first') . (take last') $ xs
    where
        sz = length xs
        first' = if first < 0 then first + sz else first
        last' = if last < 0 then last + sz else last

-- 051
paridade xs = if odd (sum (map (\x -> 1) (filter (\x -> x == True) xs))) then True else False

-- 054


-- 063
euler1 n = sum [x | x <- [1..n-1], (x `mod` 3 == 0 || x `mod` 5 == 0)]