list_gen 1 = [1]
list_gen a = if a < 1 then [a] ++ list_gen (a+1) else [a] ++ list_gen (a-1)