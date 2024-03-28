{-# LANGUAGE RecordWildCards #-}

module Src.Logic.Carona(
    gerarCarona,
    infoCarona, 
    infoCaronaByDestino,
    infoCaronaById, 
    infoCaronaByPassageiro, 
    infoCaronaByMotorista, 
    deletarCaronaPorId, 
    adicionarPassageiro, 
    removerPassageiro) where

import Src.Schema.CaronaSchema
import Src.Model.Carona
import Src.Util.Utils
import Data.List (intercalate, find)
import Debug.Trace (traceShow)
import GHC.IO (unsafePerformIO)
import System.Posix.Internals (puts)

infoCarona :: Int -> IO String
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

-- display todas as caronas com base no origem/destino, horario/dia, motoristas

-- criar carona (motorista)
gerarCarona :: String -> String -> String -> String -> String -> Double -> IO ()
gerarCarona hora date origem destino motorista valor = do
    if validarHorario hora then
        putStrLn "Horário fora do padrão requisitado!"
    else do 
        if validarData date then
            putStrLn "Data fora do padrão requisitado!"
        else do
            criarCarona (stringToTimeOfDay hora) (stringToDay date) origem destino motorista [] valor 0 [0]
            putStrLn "Carona criada com sucesso!"

-- alterar carona: adição/remoção de passageiros, status, 
adicionarPassageiro :: Int -> String -> IO String
adicionarPassageiro caronaId passageiro = do
    maybeCarona <- getCaronaById [caronaId]
    if null maybeCarona then
        return "Essa carona não existe!"
    else do
        carona <- addPassageiro (head maybeCarona) passageiro
        return (unsafePerformIO (infoCarona caronaId))

removerPassageiro :: Int -> String -> IO String
removerPassageiro caronaId passageiro = do
    maybeCarona <- getCaronaById [caronaId]
    if null maybeCarona then
        return "Essa carona não existe!"
    else do
        carona <- rmPassageiro (head maybeCarona) passageiro
        if carona == head maybeCarona then
            return "Esse passageiro não está nessa carona!"
        else do
            return (unsafePerformIO (infoCarona caronaId))