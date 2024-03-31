module Src.Logic.MotoristaLogic where

import Src.Model.MotoristaModel (Motorista (genero), confereSenha)
import Src.Schemas.Motorista (cadastraMotorista, getBy, removerMotorista, atualizarMotorista)
import Control.Monad (when)
import Src.Util.Util(validarCPF, nullOrEmpty,validarEmail,validarGenero)

cadastrarMotoristaLogic :: String -> String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
cadastrarMotoristaLogic cpf cep nome email telefone senha cnh genero
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
    | validarGenero genero = do
        putStrLn "O genero não é valido"
        return Nothing
    | otherwise = cadastraMotorista cpf cep nome email telefone senha cnh genero



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

                
buscarMotoristaLogic :: String -> String -> IO (Maybe Motorista)
buscarMotoristaLogic cpf senhaNecessaria
    | validarCPF cpf = do
        putStrLn "CPF não atende aos requisitos"
        return Nothing
    | otherwise = do
        resultado <- getBy "cpf" cpf
        case resultado of
            Just motorista -> if confereSenha motorista senhaNecessaria
                                then do
                                    return (Just motorista)
                                else do
                                    putStrLn "Senha incorreta"
                                    return Nothing
            Nothing -> do
                putStrLn "Motorista não encontrado"
                return Nothing

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
                    return resultado 
                else do
                    putStrLn "Senha incorreta"
                    return Nothing
            Nothing -> do
                putStrLn "Email não encontrado"
                return Nothing
