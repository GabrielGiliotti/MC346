revert [] = []
revert (x:xs) = revert xs ++ [x]

shift_right [] = []
shift_right (x:xs) = [head (revert (x:xs))] ++ revert(tail(revert (x:xs)))

shift_right_n _ [] = []
shift_right_n 1 (x:xs) = shift_right (x:xs)
shift_right_n n (x:xs) = shift_right_n (n-1) (shift_right (x:xs))