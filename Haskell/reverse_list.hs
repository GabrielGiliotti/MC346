revert [] = []
revert (x:xs) = (revert xs) ++ [x]