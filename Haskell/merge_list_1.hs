merge_list [] [] = []
merge_list a [] = [] 
merge_list [] b = []
merge_list (x:xs) (y:ys) = [x] ++ [y] ++ merge_list xs ys