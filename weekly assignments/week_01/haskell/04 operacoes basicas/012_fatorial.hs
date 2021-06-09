fatorial n
    | n == 0 = 1
    | otherwise = product reversedList
    where
        reversedList = reverse [1..n]

-- com fold
fatorial' n = foldl ( * ) 1 [1..n]