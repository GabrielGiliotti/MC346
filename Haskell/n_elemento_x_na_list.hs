conta_ele x [] = 0
conta_ele a (x:xs) = (if x == a then 1 else 0) + conta_ele a xs