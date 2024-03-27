module Src.Schema.CaronaSchema (
    criarCarona, deleteCaronaById, getCaronaById, getAllCaronas, selectCaronaByDestino, getCaronaByColumn, addPassageiro, removerPassageiro
) where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeOrError)
import Data.Csv (ToRecord, ToField, toRecord, record, toField, encode)
import System.IO (openFile, readFile, hClose, IOMode(AppendMode))
import qualified Data.ByteString.Lazy as BL
import Data.List.Split (splitOn)
import Src.Util.CsvHandler as Csv
import Src.Model.Carona as Carona
import GHC.IO (unsafePerformIO)
import Debug.Trace (traceShow)
import Src.Util.Utils (getCaronaAttribute)

instance ToField TimeOfDay where
    toField time = toField $ formatTime defaultTimeLocale "%H:%M:%S" time

instance ToField Day where
    toField day = toField $ formatTime defaultTimeLocale "%Y-%m-%d" day

-- Definição do estado do contador para IDs de carona
type CounterState = Int

counterState :: CounterState
counterState = 0

csvPath::String
csvPath = "./database/Caronas.csv"

-- Função para incrementar o contador de IDs de carona
incrementCounter :: CounterState -> IO CounterState
incrementCounter currentState = do
    allCaronas <- getAllCaronas
    let nextId = findNextId currentState allCaronas
    return nextId

findNextId :: CounterState -> [Carona] -> CounterState
findNextId currentId caronasList =
    if any (\u -> cid u == currentId) caronasList
        then findNextId (currentId + 1) caronasList
        else currentId

instance ToRecord Carona where
    toRecord entry = record
        [ toField (cid entry)
        , toField (hora entry)
        , toField (date entry)
        , toField (origem entry)
        , toField (destino entry)
        , toField (motorista entry)
        , toField ("" :: String)  -- Lista de passageiros vazia
        , toField (valor entry)
        , toField (avaliacaoMotorista entry)
        , toField ("" :: String)  -- Lista de avaliações de passageiros vazia
        ]

getAllCaronas :: IO [Carona]
getAllCaronas = Csv.get strToCarona csvPath

getCaronaById :: [Int] -> IO [Carona]
getCaronaById targets = do
  caronasList <- Csv.get strToCarona csvPath
  let result = filter (\u -> cid u `elem` targets) caronasList
  return result

getCaronaByColumn :: String -> String -> IO [Carona]
getCaronaByColumn att value = do
    caronas <- getAllCaronas
    let selectedCaronas = if att == "passageiros"
                          then filter (\c -> value `elem` (passageiros c)) caronas
                          else filter (\c -> getCaronaAttribute c att == value) caronas
    return selectedCaronas

deleteCaronaById :: Int -> IO ()
deleteCaronaById cidToDelete = do
    caronas <- getCaronaById [cidToDelete]
    if null caronas
        then putStrLn "Carona inexistente!" 
    else do
        delete (\c -> cid c == cidToDelete) strToCarona caronaToStr csvPath
        putStrLn "Carona deletada com sucesso!"

selectCaronaByDestino::String->IO [Carona]
selectCaronaByDestino dest = do
    allCaronas <- get parseCarona csvPath
    let result = filter (\x -> destino x == dest) allCaronas
    traceShow result $ return ()
    return result

-- Parse a line from CSV into a Carona
parseCarona :: String -> Carona
parseCarona line = case splitOn "," line of
    [cidStr, horaStr, dateStr, origem, destino, motorista, passageirosStr, valorStr, avaliacaoMotoristaStr, avaliacoesPassageirosStr] ->
        Carona {
            cid = read cidStr,
            hora = parseTimeOrError True defaultTimeLocale "%H:%M:%S" horaStr,
            date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateStr,
            origem = origem,
            destino = destino,
            motorista = motorista,
            passageiros = splitOn ";" passageirosStr,
            valor = read valorStr,
            avaliacaoMotorista = read avaliacaoMotoristaStr,
            avaliacoesPassageiros = map read (splitOn ";" avaliacoesPassageirosStr)
        }
    _ -> error "Invalid line format for Carona"

criarCarona :: TimeOfDay -> Day -> String -> String -> String -> [String] -> Double -> Int -> [Int] -> IO()
criarCarona hora dt ori dest mot pss val avMot avPss = do
    nextId <- incrementCounter counterState
    let carona = Carona nextId hora dt ori dest mot pss val avMot avPss
    append caronaToStr [carona] csvPath

updateCarona :: Carona -> Carona -> IO Carona
updateCarona carona novaCarona = do
  allCaronas <- getAllCaronas
  let updatedAllCaronas = map (\u -> if cid u == cid carona then novaCarona else u) allCaronas
  Csv.write caronaToStr updatedAllCaronas csvPath
  return novaCarona

addPassageiro :: Carona -> String -> IO Carona
addPassageiro carona passageiro = do
    let novosPassageiros = case passageiros carona of
                            [""] -> [passageiro]
                            _ -> passageiros carona ++ [passageiro]
    
        caronaAtualizada = Carona (cid carona) (hora carona) (date carona) (origem carona) (destino carona) (motorista carona) novosPassageiros (valor carona) (avaliacaoMotorista carona) (avaliacoesPassageiros carona)
    
    updateCarona carona caronaAtualizada
    return caronaAtualizada

removerPassageiro :: Carona -> String -> IO Carona
removerPassageiro carona passageiro = do
    let passageirosCarona = passageiros carona
        novosPassageiros =  filter (\passageiroIterador -> passageiro /= passageiroIterador) passageirosCarona
    
        caronaAtualizada = Carona (cid carona) (hora carona) (date carona) (origem carona) (destino carona) (motorista carona) novosPassageiros (valor carona) (avaliacaoMotorista carona) (avaliacoesPassageiros carona)
    
    updateCarona carona caronaAtualizada
    return caronaAtualizada

