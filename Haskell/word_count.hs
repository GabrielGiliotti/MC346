import Data.Char  
import System.IO

count_letters_all_words :: String -> String
count_letters_all_words string = string ++ " - " ++ show (length (string)) ++ " caracter(es)\n"

iterate_list :: [String] -> String
iterate_list [] = []
iterate_list (x:xs) = count_letters_all_words x ++ (iterate_list xs) 

main :: IO ()
main = do  
    hSetEncoding stdin utf8 --Precisei setar para utf8 para pegar caracteres especiais
    contents <- getContents
    putStrLn ("\nSaida:\nPalavras: " ++ show (length (words contents)))
    putStrLn ("\n---------------------------------\n")
    putStrLn (iterate_list (words (contents)))
    putStrLn ("---------------------------------\n\nTotal de Caracteres: " ++ show (length (concat (words contents))))