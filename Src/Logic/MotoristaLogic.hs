module Src.Logic.MotoristaLogic where

import Src.Schemas.Motorista (Motorista, cadastraMotorista, getBy, removerMotorista, atualizarMotorista)

cadastrarLogicMotorista :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
cadastrarLogicMotorista cpf cep nome email telefone senha cnh = do
    if cpf /= ""
        then if cep /= ""
            then if nome /= ""
                then if email /= ""
                    then if telefone /= ""
                        then if senha /= ""
                            then if cnh /= ""
                                then return (Just (cadastraMotorista cpf cep nome email telefone senha cnh))
                                else return (Just "CNH não informada")
                            else return (Just "Senha não informada")
                        else return (Just "Telefone não informado")
                    else return (Just "E-mail não informado")
                else return (Just "Nome não informado")
            else return (Just "CEP não informado")
        else return (Just "CPF não informado")


atualizarLogicMotorista :: String -> String -> String -> IO (Maybe Motorista)
atualizarLogicMotorista cpf coluna novoValor = do
    if cpf /= "" then
        return (Just "Cpf não pode ser vazio")
    else
        if coluna /= "Telefone" || coluna /= "Cep" || coluna /= "Senha" then
            return (Just "Escolha uma informação valida para alterar")
        else 
            return (Just atualizarMotorista cpf coluna novoValor)


removerLogicMotorista :: String -> IO (Maybe Motorista)
removerLogicMotorista cpf = do
    if cpf /= "" then
        return (Just "Cpf não pode ser vazio")
    else
        return (Just removerMotorista cpf)

buscarLogicMotorista :: String -> IO (Maybe Motorista)
buscarLogicMotorista cpf = do 
    if cpf /= "" then
        return (Just "Cpf não pode ser vazio")
    else
        return (Just (getBy "cpf" cpf))