module Src.Util.Util where

import Data.Char (isDigit, digitToInt, intToDigit, isAlphaNum, toLower)

import Text.Regex.Posix

validarCPF :: String -> Bool
validarCPF cpf
    | length cpf /= 11 = True
    | not (all isDigit cpf) = True
    | otherwise = False

nullOrEmpty :: String -> Bool
nullOrEmpty str = null (dropWhile (== ' ') str)

-- Função para validar um email sem usar expressões regulares
validarEmail :: String -> Bool
validarEmail email = not (email =~ "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")

-- Função auxiliar para dividir uma string em partes com base em um caractere
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> splitOn delim xs

generos :: [String]
generos = ["f","m"]

validarGenero :: String -> Bool
validarGenero genero = map toLower genero `notElem` generos

regioesBrasil :: [String]
regioesBrasil = ["norte", "nordeste", "centro-oeste", "sudeste", "sul"]

validaRegiao :: String -> Bool
validaRegiao regiao = map toLower regiao `notElem` regioesBrasil