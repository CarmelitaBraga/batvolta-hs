module Src.Controller.ControllerCarona (mostrarCaronasPorDestino, mostrarCaronaPorId, mostrarCaronasPassageiro, mostrarCaronasMotorista, deletarCaronaMotorista) where

import Src.Logic.Carona

mostrarCaronasPorDestino :: String -> IO String
mostrarCaronasPorDestino destino = do
    caronas <- infoCaronaByDestino destino
    return $ unlines caronas

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

deletarCaronaMotorista::String->IO ()
deletarCaronaMotorista cid = deletarCaronaPorId cid

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