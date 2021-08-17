shift_left [] = []
shift_left (x:xs) = xs ++ [x]

shift_left_n _ [] = []
shift_left_n 1 (x:xs) = shift_left (x:xs)
shift_left_n a (x:xs) = shift_left_n (a-1) (shift_left (x:xs))