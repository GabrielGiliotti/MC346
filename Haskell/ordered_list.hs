ordered [] = True
ordered [a] = True
ordered (x:y:xs) = if x <= y then ordered (y:xs) else False 