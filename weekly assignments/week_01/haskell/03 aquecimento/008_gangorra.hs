gangorra p1 c1 p2 c2
    | pc1 == pc2 = 0
    | pc1 < pc2 = 1
    | otherwise = -1
    where
        pc1 = p1 * c1
        pc2 = p2 * c2