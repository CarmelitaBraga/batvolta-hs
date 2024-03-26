{-# LANGUAGE RecordWildCards #-}

module Src.Logic.Carona(infoCarona) where

import Src.Schema.CaronaSchema
import Src.Model.Carona

import Data.List (find)

infoCarona:: Int -> IO String
infoCarona caronaId = do
    caronas <- getAllCaronas
    let maybeCarona = find (\c -> caronaId == cid c) caronas
    case maybeCarona of
        Nothing -> return "Carona not found"
        Just Carona{..} -> return $
            "Origem: " ++ origem ++ ", " ++
            "Destino: " ++ destino ++ ", " ++
            "Motorista: " ++ show motorista ++ ", " ++
            "Passageiros: " ++ show passageiros ++ ", " ++
            "Valor: " ++ show valor ++ ", " ++
            "Rate do Motorista: " ++ show avaliacaoMotorista ++ ", " ++
            "Rate dos Passageiros: " ++ show avaliacoesPassageiros


-- infoCaronaByDestino :: String -> [Carona] -> [String]
-- infoCaronaByDestino destino caronas = map infoCarona $ filter ((== destino) . destino) caronas
--     where
--         infoCarona Carona{..} = 
--             "Origem: " ++ origem ++
--             "\nDestino: " ++ destino ++
--             "\nMotorista: " ++ show motorista ++
--             "\nPassageiros: " ++ show passageiros ++
--             "\nValor: " ++ show valor ++
--             "\nAvaliação do Motorista: " ++ show avaliacaoMotorista ++
--             "\nAvaliações dos Passageiros: " ++ show avaliacoesPassageiros

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