sum_elements :: Num t => [t] -> t
sum_elements [] = 0
sum_elements (x:xs) = sum_elements_acc (x:xs) 0
  where
    sum_elements_acc :: Num v => [v] -> v -> v
    sum_elements_acc [] acc = acc
    sum_elements_acc (x:xs) acc = sum_elements_acc xs (x+acc)