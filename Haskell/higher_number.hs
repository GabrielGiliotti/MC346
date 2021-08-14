high_number [] = []
high_number (x:xs) = if [x] < (high_number xs) then high_number xs else [x]