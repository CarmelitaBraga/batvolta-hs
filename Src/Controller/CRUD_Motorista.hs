module Src.Controller.CRUD_Motorista where

import Src.Schemas.Motorista (Motorista, cadastraMotorista, getBy, removerMotorista, atualizarMotorista)



realizarCadastroMotorista :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
realizarCadastroMotorista cpf cep nome email telefone senha cnh = cadastraMotorista cpf cep nome email telefone senha cnh

cancelarCadastroMotorista :: String -> IO (Maybe Motorista)
cancelarCadastroMotorista cpf = do
    putStrLn "Cadastro cancelado com sucesso!"
    removerMotorista cpf

atualizarCadastroMotorista :: String -> String -> String -> IO (Maybe Motorista)
atualizarCadastroMotorista cpf coluna novoValor = atualizarMotorista cpf coluna novoValor

visualizarInfoCadastroMotorista :: String -> String -> IO (Maybe Motorista)
visualizarInfoCadastroMotorista campo valor =  do
    maybeMotorista <- getBy campo valor
    maybe (putStrLn "Nenhum motorista encontrado com o atributo fornecido") print maybeMotorista
    return maybeMotorista


--Crud motorista

--Informar destinos, data e horário da viagem

--Printar lista de passageiros até o momento?

--Printar destinos

--Login motorista


