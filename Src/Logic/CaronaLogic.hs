{-# LANGUAGE RecordWildCards #-}

module Src.Logic.CaronaLogic (
    gerarCarona,
    infoCarona,
    infoCaronaById,
    infoCaronaByPassageiro,
    infoCaronaByMotorista,
    infoCaronaNaoIniciadaByMotorista,
    infoCaronaEmAndamentoByMotorista,
    infoCaronaPassageirosViagemFalseByMotorista,
    deletarCaronaPorId,
    adicionarPassageiro,
    removerPassageiro,
    infoCaronaByDestino,
    existeRota,
    lugaresDisponiveis,
    infoCaronaDisponivelOriDest,
    possuiCaronaNaoIniciada,
    possuiCaronaEmAndamento,
    possuiCaronaOrigemDestino,
    possuiPassageiroViagemFalseByCarona,
    possuiPassageiroByCarona,
    iniciarCarona,
    finalizarCarona,
    mudaLimitePassageirosCarona,
    motoristaPossuiCarona,
    recusarOuAceitarPassageiro,
    caronaPertenceMotorista,
    motoristaMaisBemAvaliados
    ) where

import Src.Schemas.CaronaSchema
-- Retirar daqui posteriormente (pensar na comunicação entre Schemas)
import Src.Schemas.PassageiroViagemSchema
import Src.Model.Carona
import Src.Model.PassageiroViagem
import Src.Util.Utils
import Data.List (intercalate, find, elemIndex, elemIndices,sortOn, reverse)
import GHC.IO (unsafePerformIO)
import System.Posix.Internals (puts)
import Control.Monad (filterM)
import Debug.Trace
import Data.Char


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
    mapM (infoCarona . cid) selectedCaronas

infoCaronaById::String->IO [String]
infoCaronaById id = do
    selectedCaronas <- getCaronaByColumn "cid" id
    mapM (infoCarona . cid) selectedCaronas

infoCaronaByDestino :: String->IO [String]
infoCaronaByDestino dest = do
    selectedCaronas <- getCaronaByColumn "destinos" dest
    mapM (infoCarona . cid) selectedCaronas

infoCaronaByPassageiro::String->IO [String]
infoCaronaByPassageiro pId = do
    selectedCaronas <- getCaronaByColumn "passageiros" pId
    if null selectedCaronas then return [""]
    else mapM (infoCarona . cid) selectedCaronas

deletarCaronaPorId::String ->Int -> IO String
deletarCaronaPorId mId caronaId = do
    maybeCarona <- getOneCaronaById caronaId
    case maybeCarona of
        Just carona -> do
            if motorista carona == mId then do
                deleteCaronaById caronaId
                return "Carona cancelada com sucesso!"
            else
                return "Essa carona não está disponível para você"
        Nothing -> return "Essa carona não existe!"

infoCaronaNaoIniciadaByMotorista :: String -> IO [String]
infoCaronaNaoIniciadaByMotorista motorista = do
    selectedCaronas <- getCaronaByMotoristaEStatus motorista "NaoIniciada"
    mapM (infoCarona . cid) selectedCaronas

infoCaronaEmAndamentoByMotorista :: String -> IO [String]
infoCaronaEmAndamentoByMotorista motorista = do
    selectedCaronas <- getCaronaByMotoristaEStatus motorista "EmAndamento"
    mapM (infoCarona . cid) selectedCaronas

infoCaronaPassageirosViagemFalseByMotorista :: String -> IO [String]
infoCaronaPassageirosViagemFalseByMotorista motorista = do
    caronasMotorista <- getCaronaByColumn "motorista" motorista
    selectedCaronas <- filterM possuiPassageiroViagemFalse caronasMotorista
    mapM (infoCarona . cid) selectedCaronas

gerarCarona :: String -> String -> [String] -> String -> Double -> Int -> IO String
gerarCarona hora date destinos motorista valor numPassageirosMaximos = if validarHorario hora then
    return "Horário fora do padrão requisitado!"
else do
    if validarData date then
        return "Data fora do padrão requisitado!"
    else do
        criarCarona (stringToTimeOfDay hora) (stringToDay date) destinos motorista [] valor NaoIniciada numPassageirosMaximos
        return "Carona criada com sucesso!"

-- Função principal para verificar a disponibilidade de lugares
lugaresDisponiveis :: Carona -> IO Bool
lugaresDisponiveis carona = do
    let maximo = numPassageirosMaximos carona
        passageirosAtuais = length (passageiros carona)
        disponivel =
            if maximo == 1
                then if passageiros carona == [""] then
                    return True
                else
                    return False
            else
                return (passageirosAtuais < maximo)
    disponivel

adicionarPassageiro :: Int -> String -> IO String
adicionarPassageiro caronaId passageiro = do
    maybeCarona <- getCaronaById [caronaId]
    case maybeCarona of
        [] -> return "Essa carona não existe!"
        [carona] -> do

            if status carona == read "EmAndamento" then do
                viagem <- getViagemByCaronaPassageiro caronaId passageiro
                if aceita (head viagem) then do
                    bool <- lugaresDisponiveis carona
                    if bool then do
                        caronaAtualizada <- addPassageiro carona passageiro
                        return "Passageiro adicionado com sucesso!"

                    else if numPassageirosMaximos carona == 1
                        then return "Carona sem vagas!"
                        else return "Carona já está cheia!"
                else do
                    return "Você não foi aceito nesta carona!"
            else do
                return "Carona nao iniciada ou ja finalizada!"

removerPassageiro :: Int -> String -> IO String
removerPassageiro caronaId passageiro = do
    maybeCarona <- getCaronaById [caronaId]
    if null maybeCarona then
        return "Essa carona não existe!"
    else do
        carona <- rmPassageiro (head maybeCarona) passageiro
        if carona == head maybeCarona then
            return "Esse passageiro não está nessa carona!"
        else return "Passageiro removido com sucesso!"

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

possuiCaronaEmAndamento :: String -> IO Bool
possuiCaronaEmAndamento motorista = possuiCaronaByMotoristaEStatus motorista "EmAndamento"

possuiPassageiroViagemFalseByCarona :: Int -> IO Bool
possuiPassageiroViagemFalseByCarona caronaId = do
    maybeCarona <- getCaronaById [caronaId]
    case maybeCarona of
        [] -> return False
        (carona:_) -> possuiPassageiroViagemFalse carona

possuiPassageiroByCarona :: Int -> Int -> IO Bool
possuiPassageiroByCarona caronaId pvId = do
    maybeCarona <- getCaronaById [caronaId]
    case maybeCarona of
        [] -> return False
        (carona:_) -> possuiViagemByCaronaAndId carona pvId

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


motoristaPossuiCarona :: String -> IO Bool
motoristaPossuiCarona = possuiCaronaByMotorista

recusarOuAceitarPassageiro :: Int -> Bool -> IO String
recusarOuAceitarPassageiro pvId aceitarOuRecusar = do
    updateAceitaOuRecusaPassageiro pvId aceitarOuRecusar
    if aceitarOuRecusar then
        return "Passageiro aceito"
    else do
        deleteViagemById pvId
        return "Passageiro recusado com sucesso!"

caronaPertenceMotorista::Int->String->IO Bool
caronaPertenceMotorista idCarona idMotorista = do
    selectedCaronas <- getCaronaByColumn "motorista" idMotorista
    let caronaMotorista = filter (\c -> cid c == idCarona) selectedCaronas
    if null selectedCaronas || null caronaMotorista
        then return False
        else return True

{- PassageirosMaisBemAvaliados :: IO[]
PassageirosMaisBemAvaliados = do
    viagens <- getAllViagens
    --- caronas <- getAllCaronas
    passageiroMaiorAvaliacao <- getCaronaByColumn 
 -}
    








































































combinarViagensCaronas :: [Viagem] -> [Carona] -> [(String, Int)]
combinarViagensCaronas viagens caronas =
    [(cid c, cId v) | v <- viagens, c <- caronas, cId v == cid c]

motoristaMaisBemAvaliados :: IO[(String,Int)]
motoristaMaisBemAvaliados = do
    viagens <- getAllViagens
    caronas <- getAllCaronas
    let dadosCombinados = combinarViagensCaronas viagens caronas
        motoristasComAvaliacoes = sortOn (negate . snd) dadosCombinados
        top5Motoristas = take 5 motoristasComAvaliacoes
    return top5Motoristas