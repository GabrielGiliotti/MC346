change_old_new_once :: Eq t => t -> t -> [t] -> [t]
change_old_new_once _ _ [] = []
change_old_new_once old new (x:xs) 
  | x == old = [new] ++ xs 
  | otherwise = [x] ++ change_old_new_once old new xs