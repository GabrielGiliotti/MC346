last_element [] = []
last_element [a] = [a]
last_element (x:xs) = if tail xs /= [] then last_element xs else xs