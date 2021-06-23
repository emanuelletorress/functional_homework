isPalind s
    | foldl (flip (:)) [] s == s = True
    | otherwise = False