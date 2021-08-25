-- break_list_left retorna metade esquerda da lista
break_list_right :: (Ord v) => [v] -> [v]
break_list_right [] = []
break_list_right [c] = [c]
break_list_right (z:zs) = drop ((length (z:zs)) `div` 2) (z:zs)

-- break_list_right retorna metade direita da lista
break_list_left :: (Ord u) => [u] -> [u]
break_list_left [] = []
break_list_left [b] = [b]
break_list_left (y:ys) = take ((length (y:ys)) `div` 2) (y:ys)

-- merge_lists realiza o merge de duas listas ordenando os valores entre as duas
merge_lists :: (Ord a) => [a] -> [a] -> [a]
merge_lists [] (f:fs) = (f:fs)
merge_lists (g:gs) [] = (g:gs)
merge_lists (g:gs) (f:fs) 
  | g < f =  g:merge_lists gs (f:fs)
  | otherwise = f:merge_lists (g:gs) fs

-- merge_sort eh a funcao que deve ser executada
-- merge_sort é chamada dentro de merge_lists recursivamente, para cada metade da lista que deve ser ordenada 
merge_sort :: (Ord a) => [a] -> [a]
merge_sort []  = []
merge_sort [x] = [x]
merge_sort (x:xs) = merge_lists (merge_sort (break_list_left (x:xs))) (merge_sort (break_list_right (x:xs)))