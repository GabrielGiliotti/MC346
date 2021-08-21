change_old_new_n :: (Ord a, Num a) => a -> a -> a -> [a] -> [a]
change_old_new_n _ _ _ [] = []
change_old_new_n old new n (x:xs) 
  | (n > 0) && (x == old) = new : change_old_new_n old new (n-1) xs
  | otherwise = x : change_old_new_n old new n xs