-- caso 1
total xs = sum (map (\x -> 1) xs)

-- caso 2
total' [] = 0
total' (_:xs) = 1 + total' xs