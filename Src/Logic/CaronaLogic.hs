{-# LANGUAGE RecordWildCards #-}

module Src.Logic.CaronaLogic (
    gerarCarona,
    infoCarona, 
    infoCaronaById, 
    infoCaronaByPassageiro, 
    infoCaronaByMotorista, 
    infoCaronaNaoIniciadaByMotorista,
    deletarCaronaPorId, 
    adicionarPassageiro, 
    removerPassageiro,
    infoCaronaByDestino,
    filtrarCaronaOriDest,
    alterarStatusViagem,
    possuiCaronaNaoIniciada,
    iniciarCarona,
    finalizarCarona
    ) where

import Src.Schema.PassageiroViagemSchema
import Src.Schema.CaronaSchema
import Src.Model.Carona
import Src.Util.Utils
import Data.List (intercalate, find, elemIndex, elemIndices)
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
            "Id: " ++ show cid ++
            ", Origem: " ++ origem ++
            ", Destinos: [" ++ intercalate ", " destinos ++ "]" ++
            ", Motorista: " ++ motorista ++
            ", Passageiros: [" ++ intercalate ", " passageiros ++ "]" ++
            ", Valor: " ++ show valor ++
            ", Status: " ++ show status ++
            ", Limite de passageiros: " ++ show numPassageirosMaximos

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

infoCaronaNaoIniciadaByMotorista :: String -> IO [String]
infoCaronaNaoIniciadaByMotorista motorista = do
    selectedCaronas <- getCaronaByMotoristaEStatus motorista "NaoIniciada"
    mapM infoCarona (map cid selectedCaronas)

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

existeRota :: Carona -> String -> String -> Bool
existeRota carona o d = 
    let allStops = origem carona : destinos carona
        origIndex = elemIndex o allStops
        destIndices = elemIndices d allStops
        lastIndexD = if null destIndices then Nothing else Just (last destIndices)
    in case (origIndex, lastIndexD) of
        (Just oi, Just di) -> oi < di
        _ -> False

filtrarCaronaOriDest :: String -> String -> IO [String]
filtrarCaronaOriDest orig dest = do
    allCaronas <- getAllCaronas
    let selectedOriginCaronas = filter (\c -> origem c == orig) allCaronas
    let selectedDestinyCaronas = filter (\c -> dest `elem` (origem c : destinos c)) allCaronas
    if null selectedDestinyCaronas
        then return []
        else do
            let selectedOriginDestinyCaronas = filter (\c -> orig `elem` (origem c : destinos c)) selectedDestinyCaronas
            let selectedCaronas = filter (\c -> existeRota c orig dest) selectedOriginDestinyCaronas
            mapM (\c -> infoCarona (cid c)) selectedCaronas

alterarStatusViagem::String->Int->String->IO String
alterarStatusViagem idPassageiro idCarona resp = do
    maybeCarona <- getCaronaById [idCarona]
    if null maybeCarona then
        return "Nenhuma carona encontrada com este id."
    else updateSolicitacaoViagem idCarona idPassageiro resp

possuiCaronaNaoIniciada :: String -> IO Bool
possuiCaronaNaoIniciada motorista = possuiCaronaByMotoristaEStatus motorista "NaoIniciada"

iniciarCarona :: Int -> IO String
iniciarCarona cId = do
    maybeCarona <- getCaronaById [cId]
    if null maybeCarona then
        return "Essa carona não existe!"
    else do
        updateStatusCarona (head maybeCarona) "EmAndamento"
        return "Carona iniciada com sucesso!"

finalizarCarona :: Int -> IO String
finalizarCarona cId = do
    maybeCarona <- getCaronaById [cId]
    if null maybeCarona then
        return "Essa carona não existe!"
    else do
        updateStatusCarona (head maybeCarona) "Finalizada"
        return "Carona finalizada com sucesso!"

