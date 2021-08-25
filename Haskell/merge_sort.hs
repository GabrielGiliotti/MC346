-- break list left e right quebram a lista original ate o menor tamanho
-- para que seja reconstruida na chamada recursiva
break_list_right :: (Ord v) => [v] -> [v]
break_list_right [] = []
break_list_right [c] = [c]
break_list_right (z:zs) = drop ((length (z:zs)) `div` 2) (z:zs)

break_list_left :: (Ord u) => [u] -> [u]
break_list_left [] = []
break_list_left [b] = [b]
break_list_left (y:ys) = take ((length (y:ys)) `div` 2) (y:ys)

-- merge_lists realiza o merge de duas listas ordenando os valores entre as duas
merge_lists :: (Ord a) => [a] -> [a] -> [a]
merge_lists [] fs = fs
merge_lists gs [] = gs
merge_lists (g:gs) (f:fs) 
  | g < f =  g:merge_lists gs (f:fs)
  | otherwise = f:merge_lists (g:gs) fs

-- merge_sort deve ser chamada com a lista nao ordenada
merge_sort :: (Ord a) => [a] -> [a]
merge_sort []  = []
merge_sort [x] = [x]
merge_sort (x:xs) = merge_lists (merge_sort (break_list_left (x:xs))) (merge_sort (break_list_right (x:xs)))