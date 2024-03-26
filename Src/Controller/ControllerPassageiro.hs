module Src.Controller.ControllerPassageiro where

import Src.Schemas.SchemaPassageiro(Passageiro)
import Src.Logic.LogicPassageiro(cadastrarPassageiroLogic, removePassageiroByCpfLogic, editPassageiroCSVLogic, getPassageiroByCpfLogic)

realizarCadastroPassageiro :: String -> String -> String -> String -> String -> String -> IO (Maybe Passageiro)
realizarCadastroPassageiro nome cpf email telefone cep senha  = cadastrarPassageiroLogic nome cpf email telefone cep senha

cancelarCadastroPassageiro :: String -> IO (Maybe Passageiro)
cancelarCadastroPassageiro cpf = removePassageiroByCpfLogic cpf

atualizarCadastroPassageiro :: String -> String -> String -> String -> IO (Maybe Passageiro)
atualizarCadastroPassageiro cpf telefone cep senha = editPassageiroCSVLogic cpf telefone cep senha

visualizarInfoCadastroPassageiro :: String -> IO (Maybe Passageiro)
visualizarInfoCadastroPassageiro cpf = buscarPassageiroLogic cpf

realizarLoginPassageiro :: String -> String -> IO (Maybe Passageiro)
realizarLoginPassageiro email senha = realizarLoginPassageiroLogic email senha