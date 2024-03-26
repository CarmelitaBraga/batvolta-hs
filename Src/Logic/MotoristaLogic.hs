module Src.Logic.MotoristaLogic where

import Src.Schemas.Motorista (Motorista, cadastraMotorista, getBy, removerMotorista, atualizarMotorista, confereSenha)
import Control.Monad (when)
import Src.Util.Util(validarCPF, nullOrEmpty,validarEmail)

cadastrarMotoristaLogic :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
cadastrarMotoristaLogic cpf cep nome email telefone senha cnh
    | validarCPF cpf = do
        putStrLn "CPF não atende aos requisitos"
        return Nothing
    | nullOrEmpty cep = do
        putStrLn "CEP não pode ser vazio"
        return Nothing
    | nullOrEmpty nome = do
        putStrLn "Nome não pode ser vazio"
        return Nothing
    | validarEmail email = do
        putStrLn "E-mail não atende aos requisitos"
        return Nothing
    | nullOrEmpty telefone = do
        putStrLn "Telefone não pode ser vazio"
        return Nothing
    | nullOrEmpty senha = do
        putStrLn "Senha não pode ser vazio"
        return Nothing
    | nullOrEmpty cnh = do
        putStrLn "CNH não pode ser vazio"
        return Nothing
    | otherwise = cadastraMotorista cpf cep nome email telefone senha cnh



atualizarMotoristaLogic :: String -> String -> String -> String -> IO (Maybe Motorista)
atualizarMotoristaLogic cpf senhaNecessaria coluna novoAtributo
    | validarCPF cpf = do
        putStrLn "CPF não atende aos requisitos"
        return Nothing
    | coluna `notElem` ["Telefone", "Cep", "Senha"] = do
        putStrLn "Escolha um atributo valido para atualizar"
        return Nothing
    | otherwise = do 
        resultado <- getBy "cpf" cpf
        case resultado of
            Just motorista -> if confereSenha motorista senhaNecessaria
                                then do
                                    atualizarMotorista cpf coluna novoAtributo
                                    return (Just motorista)
                                else do
                                    putStrLn "Senha incorreta"
                                    return Nothing
            Nothing -> do
                putStrLn "Motorista não encontrado"
                return Nothing
        

removerMotoristaLogic :: String -> String -> IO (Maybe Motorista)
removerMotoristaLogic cpf senhaNecessaria
    | validarCPF cpf = do
        putStrLn "CPF não atende aos requisitos"
        return Nothing
    | otherwise = do
        resultado <- getBy "cpf" cpf
        case resultado of
            Just motorista -> if confereSenha motorista senhaNecessaria
                                then do
                                    removerMotorista cpf
                                    return (Just motorista)
                                else do
                                    putStrLn "Senha incorreta"
                                    return Nothing
            Nothing -> do
                putStrLn "Motorista não encontrado"
                return Nothing

                
buscarMotoristaLogic :: String -> IO (Maybe Motorista)
buscarMotoristaLogic cpf
    | validarCPF cpf = do
        putStrLn "CPF não atende aos requisitos"
        return Nothing
    | otherwise = do
        resultado <- getBy "cpf" cpf
        return resultado

realizarLoginMotoristaLogic :: String -> String -> IO (Maybe Motorista)
realizarLoginMotoristaLogic email senha = do
    if validarEmail email then do 
        putStrLn "Email não atende aos requisitos"
        return Nothing
    else do
        resultado <- getBy "email" email
        case resultado of
            Just motorista -> do
                if confereSenha motorista senha then do
                    putStrLn "Login efetuado com sucesso!"
                    return resultado 
                else do
                    putStrLn "Senha incorreta"
                    return Nothing
            Nothing -> do
                putStrLn "Email não encontrado"
                return Nothing


-- Pedir senha pra atualizar e remover usuario
