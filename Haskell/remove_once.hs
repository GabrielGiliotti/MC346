remove_once :: Eq t => t -> [t] -> [t]
remove_once _ [] = []
remove_once a (x:xs)
  | a /= x = [x] ++ remove_once a xs 
  | otherwise = xs