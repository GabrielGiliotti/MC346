ordered :: Ord t => [t] -> Bool
ordered [] = True
ordered [a] = True
ordered (x:y:xs) 
  | x <= y = ordered (y:xs) 
  | otherwise = False 