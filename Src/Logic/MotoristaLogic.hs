module Src.Logic.MotoristaLogic where

import Src.Schemas.Motorista (Motorista, cadastraMotorista, getBy, removerMotorista, atualizarMotorista)
import Control.Monad (when)
import Src.Util.Util(validarCPF, nullOrEmpty,validarEmail)

cadastrarLogicMotorista :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
cadastrarLogicMotorista cpf cep nome email telefone senha cnh
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



atualizarLogicMotorista :: String -> String -> String -> IO (Maybe Motorista)
atualizarLogicMotorista cpf coluna novoValor
    | validarCPF cpf = do
        putStrLn "CPF não atende aos requisitos"
        return Nothing
    | coluna `notElem` ["Telefone", "Cep", "Senha"] = do
        putStrLn "Escolha um atributo valido para atualizar"
        return Nothing
    | otherwise = atualizarMotorista cpf coluna novoValor

removerLogicMotorista :: String -> IO (Maybe Motorista)
removerLogicMotorista cpf
    | cpf == "" = return Nothing
    | otherwise = removerMotorista cpf

buscarLogicMotorista :: String -> IO (Maybe Motorista)
buscarLogicMotorista cpf
    | validarCPF cpf = do
        putStrLn "CPF não atende aos requisitos"
        return Nothing
    | otherwise = do
        resultado <- getBy "cpf" cpf
        print resultado  -- Imprime o valor retornado pela função getBy
        return Nothing
