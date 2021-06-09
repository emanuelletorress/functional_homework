elemento x xs = xs !! x'
    where
        x' = if x < 0 then x + tam else x
        tam = length xs

-- com recursao
elemento' :: Int -> [a] -> a
elemento' 0 (x:xs) = x
elemento' n (x:xs) = elemento' (e-1) xs
    where
        e = if n < 0 then (n + tam) + 1 else n
        tam = length xs