find_element a [] = False
find_element a (x:xs) = if x /= a then find_element a xs else True