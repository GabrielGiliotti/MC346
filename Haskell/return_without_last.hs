revert [] = []
revert (x:xs) = (revert xs) ++ [x]

without_last [] = []
without_last (x:xs) = revert (tail (revert (x:xs)))