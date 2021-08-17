remove _ [] = []
remove a (x:xs) = if a /= x then [x] ++ remove a xs else xs

remove_n_first _ _ [] = []
remove_n_first a n (x:xs) = if a /= x 
                            then [x] ++ (remove_n_first a n xs) 
                            else (if n > 0 then (remove_n_first a (n-1) (remove a (x:xs))) else x:xs)  