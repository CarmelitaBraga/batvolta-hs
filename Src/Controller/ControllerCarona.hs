module Src.Controller.ControllerCarona (mostrarCaronaPorId, mostrarCaronasPassageiro, mostrarCaronasMotorista, deletarCaronaMotorista) where

import Src.Logic.Carona

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