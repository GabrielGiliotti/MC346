remove_once _ [] = []
remove_once a (x:xs) = if a /= x then [x] ++ remove_once a xs else xs

revert [] = []
revert (x:xs) = revert xs ++ [x]

remove_last _ [] = []
remove_last a (x:xs) = revert (remove_once a (revert (x:xs)))