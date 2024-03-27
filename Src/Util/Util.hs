module Src.Util.Util where

    import Data.Char (isDigit, digitToInt, intToDigit, isAlphaNum)
    import Text.Regex.Posix


    validarCPF :: String -> Bool
    validarCPF cpf
        | length cpf /= 11 = True 
        | not (all isDigit cpf) = True
        | nullOrEmpty cpf = True
        | otherwise = False

    nullOrEmpty :: String -> Bool
    nullOrEmpty str = null (dropWhile (== ' ') str)

    -- Função para validar um email sem usar expressões regulares
    validarEmail :: String -> Bool
    validarEmail email = not (email =~ "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$")
