revert [] = []
revert (x:xs) = revert xs ++ [x]

shift_right [] = []
shift_right (x:xs) = [head (revert (x:xs))] ++ revert(tail(revert (x:xs)))