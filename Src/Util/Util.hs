module Src.Util.Util where

import Data.Char (isDigit, digitToInt, intToDigit, isAlphaNum)

validarCPF :: String -> Bool
validarCPF cpf
    | length cpf /= 11 = True 
    | not (all isDigit cpf) = True
    | otherwise = False

nullOrEmpty :: String -> Bool
nullOrEmpty str = null (dropWhile (== ' ') str)

-- Função para validar um email sem usar expressões regulares
validarEmail :: String -> Bool
validarEmail email =
    let parts = splitOn '@' email
    in length parts /= 2 || not (all isAlphaNum (head parts)) || '.' `notElem` (last parts)

-- Função auxiliar para dividir uma string em partes com base em um caractere
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> splitOn delim xs
