module Src.Controller.ControllerCarona (mostrarCaronasPorDestino) where

import Src.Logic.Carona

mostrarCaronasPorDestino :: String -> IO String
mostrarCaronasPorDestino destino = do
    caronas <- infoCaronaByDestino destino
    return $ unlines caronas

-- criarCaronaMotorista::String->(Maybe Carona)
-- criarCaronaMotorista = 

-- solicitarCaronaPassageiro::
-- solicitarCaronaPassageiro = 

-- responderSolicitacaoCarona::
-- responderSolicitacaoCarona = 

-- deletarCaronaMotorista::
-- deletarCaronaMotorista =

-- cancelarCaronaPassageiro::
-- cancelarCaronaPassageiro = 

-- atualizarStatusCarona::
-- atualizarStatusCarona =

-- mostrarCaronasPassageiro::
-- mostrarCaronasPassageiro =

-- mostrarCaronasMotorista::
-- mostrarCaronasMotorista =

-- mostrarCaronaPorId::
-- mostrarCaronaPorId =

-- avaliarMotoristaCarona::
-- avaliarMotoristaCarona =

-- avaliarPassageiroCarona::
-- avaliarPassageiroCarona =