module Src.Controller.ControllerCarona (
    criarViagemPassageiro,
    mostrarCaronaPorId, 
    mostrarCaronasPassageiro, 
    mostrarCaronasMotorista, 
    deletarCaronaMotorista,
    mostrarCaronasDisponiveisOrigemDestino,
    possuiCaronaNaoIniciadaController,
    infoCaronasNaoIniciadas,
    finalizarCaronaStatus,
    inicializarCaronaStatus,
    responderSolicitacaoCarona,
    criarCaronaMotorista,
    avaliarMotoristaCarona,
    modificarLimitePassageiros,
    embarcarPassageiro,
    desembarcarPassageiro,
    solicitarCaronaPassageiro,
    mostrarTrechoViagemPassageiro,
    cancelarCaronaPassageiro,
    possuiCaronasOrigemDestinoController
    ) where

import Src.Logic.CaronaLogic
import Src.Logic.PassageiroViagemLogic
import Src.Model.Carona
import Src.Util.ValidationCarona
import Control.Exception (catch)
import Data.Bool (Bool)
import Data.List (intercalate)
import Src.Schema.PassageiroViagemSchema (criarViagemPassageiro)

mostrarCaronasPassageiro::String -> IO String
mostrarCaronasPassageiro pId = do
    caronas <- infoCaronaByPassageiro pId
    return $ unlines caronas

mostrarCaronasMotorista::String->IO String
mostrarCaronasMotorista mId = do 
    caronas <- infoCaronaByMotorista mId
    return $ unlines caronas

mostrarCaronaPorId::String->IO String
mostrarCaronaPorId cid = do
    carona <- infoCaronaById cid
    return $ unlines carona

deletarCaronaMotorista::Int->IO ()
deletarCaronaMotorista cid = deletarCaronaPorId cid

mostrarCaronasDisponiveisOrigemDestino :: String -> String -> IO String
mostrarCaronasDisponiveisOrigemDestino origem destino = do
    caronas <- infoCaronaDisponivelOriDest origem destino
    if null caronas
        then return "Nenhuma carona disponível para esta rota no momento."
        else return (intercalate "\n" (map show caronas))

possuiCaronaNaoIniciadaController :: String -> IO Bool
possuiCaronaNaoIniciadaController motorista = possuiCaronaNaoIniciada motorista

possuiCaronasOrigemDestinoController :: String -> String -> IO Bool
possuiCaronasOrigemDestinoController origem destino = possuiCaronaOrigemDestino origem destino

infoCaronasNaoIniciadas :: String -> IO String
infoCaronasNaoIniciadas motorista = do
    caronas <- infoCaronaNaoIniciadaByMotorista motorista
    return $ unlines caronas

finalizarCaronaStatus :: Int -> IO String
finalizarCaronaStatus cId = finalizarCarona cId

inicializarCaronaStatus::Int -> IO String
inicializarCaronaStatus cId = iniciarCarona cId

-- Função para aceitar ou negar solicitação de carona
-- resp aceita True e False
responderSolicitacaoCarona::String->Int->String->IO()
responderSolicitacaoCarona idPassageiro idCarona resp = do
    status <- alterarStatusViagem idPassageiro idCarona resp
    putStrLn status

criarCaronaMotorista :: String -> String -> String -> [String] -> String -> Double -> Int -> IO ()
criarCaronaMotorista hora date origem destinos motorista valor limitePss = do
    let check = validateCarona valor origem destinos limitePss motorista
    if check /= "OK"
        then putStrLn $ "Erro ao criar Carona: " ++ check
        else do
            result <- gerarCarona hora date origem destinos motorista valor limitePss
            putStrLn result

avaliarMotoristaCarona::Int->String->Int->IO()
avaliarMotoristaCarona idCarona idPassageiro aval = do
    result <- avaliaMotorista idCarona idPassageiro aval
    putStrLn result

modificarLimitePassageiros::Int->String->Int->IO()
modificarLimitePassageiros idCarona idMotorista novoLimite = do
    result <- mudaLimitePassageirosCarona idCarona idMotorista novoLimite
    putStrLn result

-- verificar se tem espaço
embarcarPassageiro::Int->String->IO()
embarcarPassageiro idCarona idPassageiro = do
    result <- adicionarPassageiro idCarona idPassageiro
    putStrLn result

desembarcarPassageiro::Int->String->IO()
desembarcarPassageiro idCarona idPassageiro = do
    result <- removerPassageiro idCarona idPassageiro
    putStrLn result

-- TODO: alguma integração com notificação de motorista
solicitarCaronaPassageiro::Int -> String -> String -> String -> IO String
solicitarCaronaPassageiro idCarona idPassageiro origem destino = do
    solicitaParticiparCarona idCarona idPassageiro origem destino


mostrarTrechoViagemPassageiro::Int->String->IO()
mostrarTrechoViagemPassageiro idCarona idPassageiro = do
    result <- infoTrechoByCaronaPassageiro idCarona idPassageiro
    putStrLn result

cancelarCaronaPassageiro::Int->String->IO()
cancelarCaronaPassageiro idCarona idPassageiro = do
    result <- cancelaViagemPassageiro idCarona idPassageiro
    putStrLn result 
