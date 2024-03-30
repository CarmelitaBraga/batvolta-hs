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
    existeRota,
    lugaresDisponiveis,
    infoCaronaDisponivelOriDest,
    possuiCaronaNaoIniciada,
    possuiCaronaOrigemDestino,
    iniciarCarona,
    finalizarCarona,
    mudaLimitePassageirosCarona
    ) where

import Src.Schemas.CaronaSchema
-- Retirar daqui posteriormente (pensar na comunicação entre Schemas)
import Src.Schemas.PassageiroViagemSchema
import Src.Model.Carona
import Src.Util.Utils
import Data.List (intercalate, find, elemIndex, elemIndices)
import Debug.Trace (traceShow)
import GHC.IO (unsafePerformIO)
import System.Posix.Internals (puts)
import GHC.Exts (reallyUnsafePtrEquality)
import Control.Monad (filterM)
import Debug.Trace

infoCarona :: Int -> IO String
infoCarona caronaId = do
    caronas <- getAllCaronas
    let maybeCarona = find (\c -> caronaId == cid c) caronas
    case maybeCarona of
        Nothing -> return "Carona not found"
        Just Carona{..} -> return $
            "Id: " ++ show cid ++
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

gerarCarona :: String -> String -> [String] -> String -> Double -> Int -> IO String
gerarCarona hora date destinos motorista valor numPassageirosMaximos = do
    if validarHorario hora then
        return "Horário fora do padrão requisitado!"
    else do 
        if validarData date then
            return "Data fora do padrão requisitado!"
        else do
            criarCarona (stringToTimeOfDay hora) (stringToDay date) destinos motorista [] valor NaoIniciada numPassageirosMaximos
            return "Carona criada com sucesso!"

lugaresDisponiveis::Carona->Bool
lugaresDisponiveis carona =
    if (numPassageirosMaximos carona) > length (passageiros carona)
        then True
    else False

adicionarPassageiro :: Int -> String -> IO String
adicionarPassageiro caronaId passageiro = do
    maybeCarona <- getCaronaById [caronaId]
    if null maybeCarona then
        return "Essa carona não existe!"
    else do
        let carona = head maybeCarona
        if lugaresDisponiveis carona 
            then do
                caronaAtualizada <- addPassageiro (head maybeCarona) passageiro
                -- return (unsafePerformIO (infoCarona caronaId))
                return "Passageiro adicionado com sucesso!"
            else return "Carona sem vagas!"

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
            -- return (unsafePerformIO (infoCarona caronaId))
            return "Passageiro removido com sucesso!"

existeRota :: Carona -> String -> String -> Bool
existeRota carona o d = 
    let allStops = head (destinos carona) : destinos carona
        origIndex = elemIndex o allStops
        destIndices = elemIndices d allStops
        lastIndexD = if null destIndices then Nothing else Just (last destIndices)
    in case (origIndex, lastIndexD) of
        (Just oi, Just di) -> oi < di
        _ -> False

infoCaronaDisponivelOriDest :: String -> String -> IO [String]
infoCaronaDisponivelOriDest orig dest = do
    allCaronas <- getAllCaronas
    let selectedOriginCaronas = filter (\c -> head (destinos c)  == orig) allCaronas
    let selectedDestinyCaronas = filter (\c -> dest `elem` (head (destinos c) : destinos c)) allCaronas
    if null selectedDestinyCaronas
        then return []
        else do
            let selectedOriginDestinyCaronas = filter (\c -> orig `elem` (head (destinos c) : destinos c)) selectedDestinyCaronas
            let selectedCaronasComRota = filter (\c -> existeRota c orig dest) selectedOriginDestinyCaronas
            selectedCaronasDisponiveis <- filterM (\c -> possuiVagasDisponiveis c (getCaminho c orig dest)) selectedCaronasComRota
            mapM (infoCarona . cid) selectedCaronasDisponiveis


possuiCaronaNaoIniciada :: String -> IO Bool
possuiCaronaNaoIniciada motorista = possuiCaronaByMotoristaEStatus motorista "NaoIniciada"

possuiCaronaOrigemDestino :: String -> String -> IO Bool
possuiCaronaOrigemDestino origem destino = do
    caronas <- infoCaronaDisponivelOriDest origem destino
    return (not (null caronas))

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

mudaLimitePassageirosCarona :: Int -> String -> Int -> IO String
mudaLimitePassageirosCarona idCarona idMotorista novoLimite = do
    maybeCarona <- getCaronaById [idCarona]
    if null maybeCarona 
        then return "Essa carona não existe!"
        else do
            let carona = head maybeCarona
            if motorista carona /= idMotorista
                then return "Id do motorista não corresponde ao motorista da Carona!"
                else do
                    updateLimitePassageirosCarona carona novoLimite
                    return "Carona atualizada com sucesso!"
