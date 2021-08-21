last_element :: Eq a => [a] -> [a]
last_element [] = []
last_element [a] = [a]
last_element (x:xs) = last_element xs