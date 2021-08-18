soma_acc [] = []
soma_acc (y:ys) = cumulative_sum ys y
  where
    cumulative_sum [] acc = [acc]
    cumulative_sum (x:xs) acc = [acc] ++ cumulative_sum xs (x+acc)