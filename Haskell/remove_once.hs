remove_once _ [] = []
remove_once a (x:xs) = if a /= x then [x] ++ remove_once a xs else xs