change_old_new_all _ _ [] = []
change_old_new_all old new (x:xs) = if x == old then [new] ++ change_old_new_all old new xs else [x] ++ change_old_new_all old new xs