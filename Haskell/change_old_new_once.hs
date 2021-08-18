change_old_new_once _ _ [] = []
change_old_new_once old new (x:xs) = if x == old then [new] ++ xs else [x] ++ change_old_new_once old new xs