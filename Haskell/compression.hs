import Data.List (groupBy)

-- comprime
-- agrupa ocorrencias sequenciais em diferentes listas
agrupa_iguais :: (Eq a) => [a] -> [[a]]
agrupa_iguais [] = []
agrupa_iguais input = groupBy (==) input -- retorna [[1,1],[2,2,2]] (Lista de Listas)
                                         -- ou ["aaa", "bb", "ccc", "a"] 

-- conta quantas letras iguais existem dentro de cada lista
comprime :: (Eq a) => [a] -> [(a,Int)]
comprime xs = map (\(e:list) -> (e, length (e:list))) (agrupa_iguais xs)


-- descomprime
descomprime :: (Eq a) => [(a,Int)] -> [a]
descomprime xs = [z | (x,y) <- xs, z <- (replicate y x)]