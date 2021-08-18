change_old_new_once _ _ [] = []
change_old_new_once old new (x:xs) = if x == old then [new] ++ xs else [x] ++ change_old_new_once old new xs

change_old_new_n _ _ _ [] = []
change_old_new_n old new n (x:xs) = if n > 0 then change_old_new_n old new (n-1) (change_old_new_once old new (x:xs)) else (x:xs)