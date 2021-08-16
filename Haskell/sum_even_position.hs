sum_even_pos [] = 0
sum_even_pos (x:xs) = if xs /= [] then head xs + sum_even_pos (tail xs) else 0