import Data.Char (isSpace, toLower)

-- conta as ocorrencias de um caracter em uma string
ocorrencias :: (Foldable t, Num a) => Char -> t Char -> a
ocorrencias c string = foldl (\ acc s -> if (toLower s) /= (toLower c) then acc else acc + 1 ) 0 string

-- retorna uma lista de tuplas, onde primeiro elemento ah quantidade e segundo elemento o 
-- caracter. As tuplas podem aparecer repetidas, pois é criada uma nova para cada caracter
lista_de_tuplas :: Num a => [Char] -> [(a, Char)]
lista_de_tuplas string = map (\ c -> ((ocorrencias c string), c)) string

-- Remove tuplas criadas por espaços na string de entrada
remove_espacos :: [(a, Char)] -> [(a, Char)]
remove_espacos [] = []
remove_espacos (t:lista_tuplas)
  | isSpace (snd t) == True = remove_espacos lista_tuplas
  | otherwise = [t]++(remove_espacos lista_tuplas)

-- letramaiscomum de uma string. 
-- Com a lista de tuplas de cada caracter, removemos as tuplas que são caracteres vazios
-- e dentre o que sobraram, usamos maximum para retornar a tupla com maior ocorrencia
-- finalmente, escolhemos o segundo carater da tupla, que é o caracter desejado
letramaiscomum :: [Char] -> Char
letramaiscomum string = (snd (maximum (remove_espacos (lista_de_tuplas string))))