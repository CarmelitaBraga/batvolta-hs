module Src.Controller.ControllerMotorista where

import Src.Model.MotoristaModel (Motorista (cpf))
import Src.Logic.MotoristaLogic (cadastrarMotoristaLogic,atualizarMotoristaLogic,removerMotoristaLogic,buscarMotoristaLogic,realizarLoginMotoristaLogic)
import Src.Schemas.Notificacao (getBy,insereNotificacao)
import Src.Model.NotificacaoModel (Notificacao)


realizarCadastroMotorista :: String -> String -> String -> String -> String -> String -> String -> String -> IO (Maybe Motorista)
realizarCadastroMotorista = cadastrarMotoristaLogic

cancelarCadastroMotorista :: String -> String -> IO (Maybe Motorista)
cancelarCadastroMotorista = removerMotoristaLogic

atualizarCadastroMotorista :: String -> String -> String -> String -> IO (Maybe Motorista)
atualizarCadastroMotorista= atualizarMotoristaLogic 

visualizarInfoCadastroMotorista :: String -> String-> IO (Maybe Motorista)
visualizarInfoCadastroMotorista = buscarMotoristaLogic

realizarLoginMotorista :: String -> String -> IO (Maybe Motorista)
realizarLoginMotorista = realizarLoginMotoristaLogic

carregaNotificacoes :: String -> IO [Notificacao]
carregaNotificacoes = getBy

adicionarNotificacao :: String -> String -> Int -> String -> IO (Maybe Notificacao)
adicionarNotificacao = insereNotificacao