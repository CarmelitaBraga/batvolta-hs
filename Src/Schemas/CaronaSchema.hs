module Src.Schemas.CaronaSchema (
    criarCarona,
    deleteCaronaById,
    getCaronaById,
    getOneCaronaById,
    getAllCaronas,
    getCaronaByDestino,
    getCaronaByColumn,
    getCaminho,
    addPassageiro,
    rmPassageiro,
    getCaronaByOrigem,
    getCaronaByMotoristaEStatus,
    possuiCaronaByMotoristaEStatus,
    possuiCaronaByMotorista,
    updateStatusCarona,
    updateLimitePassageirosCarona
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
import GHC.IO (unsafePerformIO)
import Src.Util.Utils (retornaSubLista, getCaronaAttribute, getViagemAttribute)
import Src.Model.Carona
import Src.Model.PassageiroViagem
import Debug.Trace

instance ToField TimeOfDay where
    toField time = toField $ formatTime defaultTimeLocale "%H:%M" time

instance ToField Day where
    toField day = toField $ formatTime defaultTimeLocale "%d/%m/%Y" day

caronaCsvPath::String
caronaCsvPath = "./database/Caronas.csv"

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
        , toField ("" :: String)
        , toField (motorista entry)
        , toField ("" :: String)
        , toField (status entry)
        , toField (numPassageirosMaximos entry)
        ]

getAllCaronas :: IO [Carona]
getAllCaronas = Csv.get strToCarona caronaCsvPath

getCaronaById :: [Int] -> IO [Carona]
getCaronaById targets = do
  caronasList <- Csv.get strToCarona caronaCsvPath
  let result = filter (\u -> cid u `elem` targets) caronasList
  return result

getOneCaronaById :: Int -> IO (Maybe Carona)
getOneCaronaById caronaId = do
    caronasList <- Csv.get strToCarona caronaCsvPath
    let usuarioEncontrado = filter (\u -> cid u == caronaId) caronasList
    case usuarioEncontrado of
        [u] -> return (Just u)
        _ -> return Nothing

getCaronaByColumn :: String -> String -> IO [Carona]
getCaronaByColumn att value = do
    caronas <- getAllCaronas
    let selectedCaronas
            | att == "passageiros" = filter (\c -> value `elem` passageiros c) caronas
            | att == "destinos" = filter (\c -> value `elem` destinos c) caronas
            | otherwise = filter (\c -> getCaronaAttribute c att == value) caronas
    return selectedCaronas

getCaronaByMotoristaEStatus :: String -> String -> IO [Carona]
getCaronaByMotoristaEStatus motorista statusBuscado = do
    caronasMotorista <- getCaronaByColumn "motorista" motorista
    let selectedCaronas = filter (\c -> read statusBuscado == status c) caronasMotorista
    return selectedCaronas

deleteCaronaById :: Int -> IO ()
deleteCaronaById cidToDelete = do
    caronas <- getCaronaById [cidToDelete]
    delete (\c -> cid c == cidToDelete) strToCarona caronaToStr caronaCsvPath

-- Parse a line from CSV into a Carona
parseCarona :: String -> Carona
parseCarona line = case splitOn "," line of
    [cidStr, horaStr, dateStr, destinosStr, motorista, passageirosStr, valorStr, statusStr, numPassageirosMaximos] ->
        Carona {
            cid = read cidStr,
            hora = parseTimeOrError True defaultTimeLocale "%H:%M" horaStr,
            date = parseTimeOrError True defaultTimeLocale "%d/%m/%Y" dateStr,
            destinos = splitOn ";" destinosStr,
            motorista = motorista,
            passageiros = splitOn ";" passageirosStr,
            valor = read valorStr,
            status = read statusStr,
            numPassageirosMaximos = read numPassageirosMaximos
        }
    _ -> error $ "Invalid line format for Carona: " ++ line

criarCarona :: TimeOfDay -> Day -> [String] -> String -> [String] -> Double -> StatusCarona -> Int -> IO ()
criarCarona hora dt dest mot pss val status numPss = do
    nextId <- incrementCounter counterState
    let carona = Carona nextId hora dt dest mot pss val status numPss
    append caronaToStr [carona] caronaCsvPath

updateCarona :: Carona -> Carona -> IO Carona
updateCarona carona novaCarona = do
  allCaronas <- getAllCaronas
  let updatedAllCaronas = map (\u -> if cid u == cid carona then novaCarona else u) allCaronas
  Csv.write caronaToStr updatedAllCaronas caronaCsvPath
  return novaCarona

addPassageiro :: Carona -> String -> IO Carona
addPassageiro carona passageiro = do
    let novosPassageiros = case passageiros carona of
                            [""] -> [passageiro]
                            _ -> passageiros carona ++ [passageiro]

        caronaAtualizada = Carona (cid carona) (hora carona) (date carona) (destinos carona) (motorista carona) novosPassageiros (valor carona) (status carona) (numPassageirosMaximos carona)

    updateCarona carona caronaAtualizada
    return caronaAtualizada

rmPassageiro :: Carona -> String -> IO Carona
rmPassageiro carona passageiro = do
    let passageirosCarona = passageiros carona
        novosPassageiros =  filter (/= passageiro) passageirosCarona

        caronaAtualizada = Carona (cid carona) (hora carona) (date carona) (destinos carona) (motorista carona) novosPassageiros (valor carona) (status carona) (numPassageirosMaximos carona)
    updateCarona carona caronaAtualizada
    return caronaAtualizada

getCaronaByOrigem::String->IO [Carona]
getCaronaByOrigem orig = do
    allCaronas <- get parseCarona caronaCsvPath
    let result = filter (\c -> head (destinos c) == orig) allCaronas
    return result

getCaronaByDestino::String->IO [Carona]
getCaronaByDestino dest = do
    allCaronas <- get parseCarona caronaCsvPath
    let result = filter (\c -> dest `elem` destinos c) allCaronas
    return result

-- Function to determine the new status
determineStatus :: String -> StatusCarona -> StatusCarona
determineStatus status oldStatus =
    case status of
        "NaoIniciada" -> NaoIniciada
        "EmAndamento" -> EmAndamento
        "Finalizada" -> Finalizada
        _ -> oldStatus

possuiCaronaByMotorista :: String -> IO Bool
possuiCaronaByMotorista motorista = do
    caronas <- getCaronaByColumn "motorista" motorista
    return (not (null caronas))

possuiCaronaByMotoristaEStatus :: String -> String -> IO Bool
possuiCaronaByMotoristaEStatus motorista statusStr = do
    caronas <- getCaronaByMotoristaEStatus motorista statusStr
    return (not (null caronas))

-- Updated function
updateStatusCarona::Carona->String->IO Carona
updateStatusCarona carona newStatusStr = do
    let newStatus = determineStatus newStatusStr (status carona)
    let novaCarona = Carona (cid carona) (hora carona) (date carona) (destinos carona) (motorista carona) (passageiros carona) (valor carona) newStatus (numPassageirosMaximos carona)
    updateCarona carona novaCarona
    return novaCarona

updateLimitePassageirosCarona::Carona->Int->IO Carona
updateLimitePassageirosCarona carona novoLimitePss = do
    let novaCarona = Carona (cid carona) (hora carona) (date carona) (destinos carona) (motorista carona) (passageiros carona) (valor carona) (status carona) novoLimitePss
    updateCarona carona novaCarona
    return novaCarona

getCaminho :: Carona -> String -> String -> [String]
getCaminho carona origem destino = do
   let caminho = destinos carona
   retornaSubLista caminho origem destino