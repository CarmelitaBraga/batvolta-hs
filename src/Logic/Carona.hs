module Logic.Carona where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Logic.Passageiro (Passageiro)
import Logic.Motorista (Motorista)

data carona = carona {
    id :: Int,
    hora :: TimeOfDay,
    data :: Day,
    origem :: String,
    destino :: String,
    motorista :: Motorista,
    passageiros :: [passageiro],
    valor :: Double,
    avaliacaoMotorista :: Int,
    avaliacoesPassageiros :: [Int]
} deriving (Show)

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


-- display todas as caronas com base no origem/destino, horario/dia, motoristas

-- criar carona (motorista)