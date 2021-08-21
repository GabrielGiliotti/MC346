last_element2 [] = []
last_element2 [a] = [a]
last_element2 (x:xs) = last_element2 xs