module Src.Logic.Carona() where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Logic.Passageiro (Passageiro)
import Logic.Motorista (Motorista)

-- informações sobre a carona
infoCarona::Carona->String
infoCarona Carona{..} = 
    "Origem: " ++ origem ++
    "\nDestino: " ++ destino ++
    "\nMotorista: " ++ show motorista ++
    "\nPassageiros: " ++ show passageiros ++
    "\nValor: " ++ show valor ++
    "\nAvaliação do Motorista: " ++ show avaliacaoMotorista ++
    "\nAvaliações dos Passageiros: " ++ show avaliacoesPassageiros
    
-- cancelar carona
    -- motorista
    -- passageiros

-- solicitar carona

-- aceitar carona

-- alterar carona: adição/remoção de passageiros, status, 

-- display todas as caronas com base no origem/destino, horario/dia, motoristas

-- criar carona (motorista)
-- criarCarona :: TimeOfDay -> Day -> String -> String -> Motorista -> Double -> IO String
-- criarCarona hora data origem destino motorista valor = do