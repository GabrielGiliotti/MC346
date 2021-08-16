merge_list [] [] = []
merge_list a [] = a 
merge_list [] b = b
merge_list (x:xs) (y:ys) = [x] ++ [y] ++ merge_list xs ys