module Src.Schemas.PassageiroViagemSchema (
    criarViagemPassageiro, 
    getAllViagens, 
    getViagemById, 
    deleteViagemById, 
    getViagemByColumn,
    updateViagem,
    updateSolicitacaoViagem,
    updateAvaliacaoViagem,
    possuiVagasDisponiveis,
    getViagemByCaronaPassageiro
) where

import Data.Time.Format
import Data.Csv (ToRecord, ToField, toRecord, record, toField, encode)
import System.IO (openFile, readFile, hClose, IOMode(AppendMode))
import Data.Time
import qualified Data.ByteString.Lazy as BL
import Src.Util.CsvHandler as Csv
import GHC.IO (unsafePerformIO)
import Src.Util.Utils (getViagemAttribute)
import Src.Model.Carona
import Src.Model.PassageiroViagem
import Data.Char (toLower)
import Debug.Trace

stringToBool :: String -> Bool
stringToBool s 
   | b == "true"  = True
   | b == "false" = False
   | otherwise    = False
   where b = map toLower s


viagemCsvPath::String
viagemCsvPath = "./database/ViagemPassageiros.csv"

type CounterStateViagem = Int

counterStateV :: CounterStateViagem
counterStateV = 0

-- Função para incrementar o contador de IDs de carona
incrementCounterV :: CounterStateViagem -> IO CounterStateViagem
incrementCounterV currentState = do
    allViagens <- getAllViagens
    let nextId = findNextIdV currentState allViagens
    return nextId

findNextIdV :: CounterStateViagem -> [PassageiroViagem] -> CounterStateViagem
findNextIdV currentId viagensList =
    if any (\u -> pid u == currentId) viagensList
        then findNextIdV (currentId + 1) viagensList
        else currentId

criarViagemPassageiro :: Int -> Bool -> [String] -> Int -> String -> IO()
criarViagemPassageiro c ack cam aval psId = do
    nextId <- incrementCounterV counterStateV
    let viagem = PassageiroViagem nextId c ack cam aval psId
    append viagemToStr [viagem] viagemCsvPath

getAllViagens::IO [PassageiroViagem]
getAllViagens = Csv.get strToViagem viagemCsvPath

getViagemById :: [Int] -> IO [PassageiroViagem]
getViagemById targets = do
  viagensList <- Csv.get strToViagem viagemCsvPath
  let result = filter (\u -> pid u `elem` targets) viagensList
  return result

deleteViagemById :: Int -> IO ()
deleteViagemById pidToDelete = do
    viagens <- getViagemById [pidToDelete]
    if null viagens
        then putStrLn "Viagem inexistente!" 
    else do
        delete (\c -> pid c == pidToDelete) strToViagem viagemToStr viagemCsvPath
        putStrLn "Viagem deletada com sucesso!"

getViagemByColumn :: String -> String -> IO [PassageiroViagem]
getViagemByColumn att value = do
    viagens <- getAllViagens
    let selectedViagens
            | att == "caminho" = filter (\v -> value `elem` caminho v) viagens
            | otherwise = filter (\v -> getViagemAttribute v att == value) viagens
    return selectedViagens

getViagemByCaronaPassageiro :: Int -> String -> IO [PassageiroViagem]
getViagemByCaronaPassageiro idCarona idPassageiro = do
    maybeViagensPss <- getViagemByColumn "passageiroId" idPassageiro
    let selectedViagens = filter (\c -> cId c == idCarona) maybeViagensPss
    return selectedViagens

updateViagem :: PassageiroViagem->PassageiroViagem->IO PassageiroViagem
updateViagem viagem novaViagem = do
  allViagens <- getAllViagens
  let updatedAllViagens = map (\u -> if pid u == pid viagem then novaViagem else u) allViagens
  Csv.write viagemToStr updatedAllViagens viagemCsvPath
  return novaViagem

updateSolicitacaoViagem :: Int -> String -> String -> IO String
updateSolicitacaoViagem caronaId passageiroId status = do
    maybeViagem <- getViagemByCaronaPassageiro caronaId passageiroId
    if null maybeViagem then
        return "Registro de carona de passageiro inexistente!"
    else do
        let viagem = head maybeViagem
        let novaViagem = PassageiroViagem
                            (pid viagem)
                            (cId viagem)
                            (stringToBool status)
                            (caminho viagem)
                            (avaliacaoMtrst viagem)
                            passageiroId
        updateViagem viagem novaViagem
        return "Status de Carona de Passageiro alterado com sucesso!"

updateAvaliacaoViagem::Int->String->Int->IO String
updateAvaliacaoViagem idCarona idPassageiro nota = do
    maybeViagem <- getViagemByCaronaPassageiro idCarona idPassageiro
    if null maybeViagem then
        return "Registro de carona de passageiro inexistente!"
    else do
        let viagem = head maybeViagem
        let novaViagem = PassageiroViagem (pid viagem) (cId viagem) (aceita viagem) (caminho viagem) nota (passageiroId viagem)
        updateViagem viagem novaViagem
        return "Motorista avaliado com sucesso!"

possuiVagasDisponiveis :: Carona -> [String] -> IO Bool
possuiVagasDisponiveis carona caminho = do 
    passageirosNoCaminho <- getPassageirosNoCaminho (cid carona) caminho
    return (length passageirosNoCaminho < numPassageirosMaximos carona)

getPassageirosNoCaminho :: Int -> [String] -> IO [PassageiroViagem]
getPassageirosNoCaminho cId caminhoTotal = do
    passageirosDaCarona <- getViagemByColumn "cid" (show cId)
    let passageirosNoCaminho = filter (\passageiro ->
                                         let caminhoPassageiroSemUltimo = init (caminho passageiro)
                                         in any (`elem` caminhoTotal) caminhoPassageiroSemUltimo
                                      ) passageirosDaCarona
    return passageirosNoCaminho


