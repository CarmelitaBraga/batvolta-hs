module Src.Controller.ControllerMotorista where

import Src.Schemas.Motorista(Motorista)
import Src.Logic.MotoristaLogic (cadastrarLogicMotorista,atualizarLogicMotorista,removerLogicMotorista,buscarLogicMotorista)


realizarCadastroMotorista :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
realizarCadastroMotorista cpf cep nome email telefone senha cnh = cadastrarLogicMotorista cpf cep nome email telefone senha cnh

cancelarCadastroMotorista :: String -> IO (Maybe Motorista)
cancelarCadastroMotorista cpf = removerLogicMotorista cpf

atualizarCadastroMotorista :: String -> String -> String -> IO (Maybe Motorista)
atualizarCadastroMotorista cpf atributo novoValor = atualizarLogicMotorista cpf atributo novoValor

visualizarInfoCadastroMotorista :: String -> IO (Maybe Motorista)
visualizarInfoCadastroMotorista cpf = buscarLogicMotorista cpf



--Login motorista


