module Src.Controller.ControllerCarona (
    mostrarCaronaPorId, 
    mostrarCaronasPassageiro, 
    mostrarCaronasMotorista, 
    deletarCaronaMotorista,
    mostrarCaronasOrigemDestino
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

-- criarCaronaMotorista::String->(Maybe Carona)
-- criarCaronaMotorista = 

-- solicitarCaronaPassageiro::
-- solicitarCaronaPassageiro = 

-- responderSolicitacaoCarona::
-- responderSolicitacaoCarona = 

-- cancelarCaronaPassageiro::
-- cancelarCaronaPassageiro = 

-- atualizarStatusCarona::
-- atualizarStatusCarona =

-- avaliarMotoristaCarona::
-- avaliarMotoristaCarona =

-- avaliarPassageiroCarona::
-- avaliarPassageiroCarona =