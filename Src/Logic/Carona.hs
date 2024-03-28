{-# LANGUAGE RecordWildCards #-}

module Src.Logic.Carona(
    gerarCarona,
    infoCarona, 
    infoCaronaById, 
    infoCaronaByPassageiro, 
    infoCaronaByMotorista, 
    deletarCaronaPorId, 
    adicionarPassageiro, 
    removerPassageiro,
    infoCaronaByDestino
    ) where

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
            ", Destinos: [" ++ intercalate ", " destinos ++ "]" ++
            ", Motorista: " ++ motorista ++
            ", Passageiros: [" ++ intercalate ", " passageiros ++ "]" ++
            ", Valor: " ++ show valor ++
            ", Status: " ++ show status ++
            ", Número de passageiros máximos: " ++ show numPassageirosMaximos

infoCaronaByMotorista::String->IO [String]
infoCaronaByMotorista mId = do
    selectedCaronas <- getCaronaByColumn "motorista" mId
    mapM infoCarona (map cid selectedCaronas)

infoCaronaById::String->IO [String]
infoCaronaById id = do
    selectedCaronas <- getCaronaByColumn "cid" id
    mapM infoCarona (map cid selectedCaronas)

infoCaronaByDestino :: String->IO [String]
infoCaronaByDestino dest = do
    selectedCaronas <- getCaronaByColumn "destinos" dest

    mapM infoCarona (map cid selectedCaronas)
infoCaronaByPassageiro::String->IO [String]
infoCaronaByPassageiro pId = do
    selectedCaronas <- getCaronaByColumn "passageiros" pId
    mapM infoCarona (map cid selectedCaronas)

deletarCaronaPorId::Int -> IO ()
deletarCaronaPorId caronaId = do
    maybeCarona <- getCaronaById [caronaId]
    if null maybeCarona then
        putStrLn "Essa carona não existe!"
    else do
        deleteCaronaById caronaId
        putStrLn "Carona deletada com sucesso!"

-- cancelar carona
    -- motorista
    -- passageiros

-- solicitar carona

-- aceitar carona

-- display todas as caronas com base no origem/destino, horario/dia, motoristas

-- criar carona (motorista)
gerarCarona :: String -> String -> String -> [String] -> String -> Double -> Int -> IO ()
gerarCarona hora date origem destinos motorista valor numPassageirosMaximos = do
    if validarHorario hora then
        putStrLn "Horário fora do padrão requisitado!"
    else do 
        if validarData date then
            putStrLn "Data fora do padrão requisitado!"
        else do
            criarCarona (stringToTimeOfDay hora) (stringToDay date) origem destinos motorista [] valor NaoIniciada numPassageirosMaximos
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