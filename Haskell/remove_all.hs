remove_all _ [] = []
remove_all a (x:xs) = if a /= x then [x] ++ remove_all a xs else remove_all a xs