module Src.Schema.CaronaSchema (
    criarCarona, deleteCaronaById, getCaronaById, getAllCaronas, getCaronaByColumn, addPassageiro, rmPassageiro
) where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Format
import Data.Csv (ToRecord, ToField, toRecord, record, toField, encode)
import System.IO (openFile, readFile, hClose, IOMode(AppendMode))
import Data.Time
import qualified Data.ByteString.Lazy as BL
import Data.List.Split (splitOn)
import Src.Util.CsvHandler as Csv
import Src.Model.Carona as Carona
import GHC.IO (unsafePerformIO)
import Debug.Trace (traceShow)
import Src.Util.Utils (getCaronaAttribute)
import Src.Model.Carona (Carona)

instance ToField TimeOfDay where
    toField time = toField $ formatTime defaultTimeLocale "%H:%M" time

instance ToField Day where
    toField day = toField $ formatTime defaultTimeLocale "%d/%m/%Y" day

-- Instância ToField para StatusCarona
instance ToField StatusCarona where
  toField status =
    case status of
      NaoIniciada -> toField ("NaoIniciada" :: String)
      EmAndamento -> toField ("EmAndamento" :: String)
      Finalizada -> toField ("Finalizada" :: String)
            
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
        , toField ("" :: String)
        , toField (motorista entry)
        , toField ("" :: String)  -- Lista de passageiros vazia
        , toField (status entry)
        , toField (numPassageirosMaximos entry)
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
    delete (\c -> cid c == cidToDelete) strToCarona caronaToStr csvPath

-- Parse a line from CSV into a Carona
parseCarona :: String -> Carona
parseCarona line = case splitOn "," line of
    [cidStr, horaStr, dateStr, origem, destinos, motorista, passageirosStr, valorStr, statusStr, numPassageirosMaximos] ->
        Carona {
            cid = read cidStr,
            hora = parseTimeOrError True defaultTimeLocale "%H:%M" horaStr,
            date = parseTimeOrError True defaultTimeLocale "%d/%m/%Y" dateStr,
            origem = origem,
            destinos = splitOn ";" destinos,
            motorista = motorista,
            passageiros = splitOn ";" passageirosStr,
            valor = read valorStr,
            status = read statusStr,
            numPassageirosMaximos = read numPassageirosMaximos
        }
    _ -> error "Invalid line format for Carona"

criarCarona :: TimeOfDay -> Day -> String -> [String] -> String -> [String] -> Double -> StatusCarona -> Int -> IO ()
criarCarona hora dt ori dest mot pss val status numPss = do
    nextId <- incrementCounter counterState
    let carona = Carona nextId hora dt ori dest mot pss val status numPss
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
    
        caronaAtualizada = Carona (cid carona) (hora carona) (date carona) (origem carona) (destinos carona) (motorista carona) novosPassageiros (valor carona) (status carona) (numPassageirosMaximos carona)
    
    updateCarona carona caronaAtualizada
    return caronaAtualizada

rmPassageiro :: Carona -> String -> IO Carona
rmPassageiro carona passageiro = do
    let passageirosCarona = passageiros carona
        novosPassageiros =  filter (/= passageiro) passageirosCarona
    
        caronaAtualizada = Carona (cid carona) (hora carona) (date carona) (origem carona) (destinos carona) (motorista carona) novosPassageiros (valor carona) (status carona) (numPassageirosMaximos carona)
    
    updateCarona carona caronaAtualizada
    return caronaAtualizada

