sum_even [] = 0
sum_even (x:xs) = (if mod x 2 /= 0 then 0 else x) + sum_even xs