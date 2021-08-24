return_pos ::  (Eq a, Num a) => a -> [a] -> [a]
return_pos _ [] = []
return_pos n (x:xs) = return_pos_aux n (x:xs) 0

return_pos_aux :: (Eq b, Num b) => b -> [b] -> b -> [b]
return_pos_aux _ [] acc = []
return_pos_aux i (y:ys) acc
  | i == y = (1+acc):(return_pos_aux i ys (1+acc)) 
  | otherwise = return_pos_aux i ys (1+acc)   