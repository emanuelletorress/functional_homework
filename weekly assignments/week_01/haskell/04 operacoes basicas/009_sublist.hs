sublist first last xs = drop first' . take last' $ xs
    where
        sz = length xs
        first' = if first < 0 then first + sz else first
        last' = if last < 0 then last + sz else last