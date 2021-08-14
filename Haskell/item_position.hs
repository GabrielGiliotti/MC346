find_element a [] = False
find_element a (x:xs) = if x /= a then find_element a xs else True

item_position a [] = 0
item_position a (x:xs) = if find_element a (x:xs) 
                         then (if a /= x then (item_position a xs) + 1 else (item_position a []) + 1) 
                         else 0