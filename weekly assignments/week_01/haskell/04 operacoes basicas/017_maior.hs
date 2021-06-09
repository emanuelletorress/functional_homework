maior [x] = x
maior (x:xs) = if x > biggest then x else biggest
    where biggest = maior xs

-- fold
maior' xs = foldr (\x y -> if x > y then x else y) 0 xs
