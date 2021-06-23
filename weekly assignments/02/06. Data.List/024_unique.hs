unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)