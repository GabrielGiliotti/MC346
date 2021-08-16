list_gen 1 = [1]
list_gen a = if a < 1 then list_gen (a+1) ++ [a] else list_gen (a-1) ++ [a]