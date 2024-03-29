module Src.Controller.ControllerCarona (
    mostrarCaronaPorId, 
    mostrarCaronasPassageiro, 
    mostrarCaronasMotorista, 
    deletarCaronaMotorista,
    mostrarCaronasOrigemDestino,
    finalizarCaronaStatus,
    inicializarCaronaStatus,
    responderSolicitacaoCarona
    ) where

import Src.Logic.CaronaLogic

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

mostrarCaronasOrigemDestino :: String -> String -> IO ()
mostrarCaronasOrigemDestino origem destino = do
    caronas <- filtrarCaronaOriDest origem destino
    if null caronas
        then putStrLn "Nenhuma carona disponível para esta rota no momento."
        else mapM_ putStrLn caronas

finalizarCaronaStatus::Int->IO()
finalizarCaronaStatus pId = do
    msg <- alterarStatusCarona pId "Finalizada"
    putStrLn msg

inicializarCaronaStatus::Int->IO()
inicializarCaronaStatus pId = do
    msg <- alterarStatusCarona pId "EmAndamento"
    putStrLn msg

-- Função para aceitar ou negar solicitação de carona
-- resp aceita True e False
responderSolicitacaoCarona::String->Int->String->IO()
responderSolicitacaoCarona idPassageiro idCarona resp = do
    status <- alterarStatusViagem idPassageiro idCarona resp
    putStrLn status

-- criarCaronaMotorista::String->(Maybe Carona)
-- criarCaronaMotorista = 

-- solicitarCaronaPassageiro::
-- solicitarCaronaPassageiro = 

-- cancelarCaronaPassageiro::
-- cancelarCaronaPassageiro = 

-- atualizarStatusCarona::
-- atualizarStatusCarona =

-- avaliarMotoristaCarona::
-- avaliarMotoristaCarona =

-- avaliarPassageiroCarona::
-- avaliarPassageiroCarona =