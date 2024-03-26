module Src.Controller.ControllerMotorista where

import Src.Schemas.Motorista(Motorista)
import Src.Logic.MotoristaLogic (cadastrarMotoristaLogic,atualizarMotoristaLogic,removerMotoristaLogic,buscarMotoristaLogic,realizarLoginMotoristaLogic)


realizarCadastroMotorista :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
realizarCadastroMotorista cpf cep nome email telefone senha cnh = cadastrarMotoristaLogic cpf cep nome email telefone senha cnh

cancelarCadastroMotorista :: String -> IO (Maybe Motorista)
cancelarCadastroMotorista cpf = removerMotoristaLogic cpf

atualizarCadastroMotorista :: String -> String -> String -> IO (Maybe Motorista)
atualizarCadastroMotorista cpf atributo novoValor = atualizarMotoristaLogic cpf atributo novoValor

visualizarInfoCadastroMotorista :: String -> IO (Maybe Motorista)
visualizarInfoCadastroMotorista cpf = buscarMotoristaLogic cpf

realizarLoginMotorista :: String -> String -> IO (Maybe Motorista)
realizarLoginMotorista email senha = realizarLoginMotoristaLogic email senha

