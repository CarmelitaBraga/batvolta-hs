{-# LANGUAGE RecordWildCards #-}

module Src.Logic.Carona(infoCarona, infoCaronaByDestino, infoCaronaById, infoCaronaByPassageiro, infoCaronaByMotorista, deletarCaronaPorId) where

import Src.Schema.CaronaSchema
import Src.Model.Carona

import Data.List (find)
import Data.List (intercalate)
import Debug.Trace (traceShow)

infoCarona:: Int -> IO String
infoCarona caronaId = do
    caronas <- getAllCaronas
    let maybeCarona = find (\c -> caronaId == cid c) caronas
    case maybeCarona of
        Nothing -> return "Carona not found"
        Just Carona{..} -> return $
            "Origem: " ++ origem ++
            ", Destino: " ++ destino ++
            ", Motorista: " ++ motorista ++
            ", Passageiros: [" ++ intercalate ", " passageiros ++ "]" ++
            ", Valor: " ++ show valor ++
            ", Rate do Motorista: " ++ show avaliacaoMotorista ++
            ", Rate dos Passageiros: [" ++ intercalate ", " (map show avaliacoesPassageiros) ++ "]"

infoCaronaByDestino :: String->IO [String]
infoCaronaByDestino dest = do
    selectedCaronas <- getCaronaByColumn "destino" dest
    mapM infoCarona (map cid selectedCaronas)

infoCaronaByMotorista::String->IO [String]
infoCaronaByMotorista mId = do
    selectedCaronas <- getCaronaByColumn "motorista" mId
    mapM infoCarona (map cid selectedCaronas)

infoCaronaById::String->IO [String]
infoCaronaById id = do
    selectedCaronas <- getCaronaByColumn "cid" id
    mapM infoCarona (map cid selectedCaronas)

infoCaronaByPassageiro::String->IO [String]
infoCaronaByPassageiro pId = do
    selectedCaronas <- getCaronaByColumn "passageiros" pId
    mapM infoCarona (map cid selectedCaronas)

deletarCaronaPorId::String->IO ()
deletarCaronaPorId id = deleteCaronaById (read id)

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