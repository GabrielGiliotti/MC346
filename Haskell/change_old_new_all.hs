change_old_new_all :: Eq t => t -> t -> [t] -> [t]
change_old_new_all _ _ [] = []
change_old_new_all old new (x:xs) 
  | x == old = [new] ++ change_old_new_all old new xs 
  | otherwise = [x] ++ change_old_new_all old new xs