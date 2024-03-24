module Src.Schema.CaronaSchema (
    criarCarona, apagarCarona
) where

import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeOrError)
import Data.Csv (ToRecord, ToField, toRecord, record, toField, encode)
import Src.Util.CsvHandler as Csv
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import System.IO (openFile, readFile, hClose, IOMode(AppendMode))
import qualified Data.ByteString.Lazy as BL

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

data Carona = Carona {
    cid :: Int,
    hora :: TimeOfDay,
    date :: Day,
    origem :: String,
    destino :: String,
    motorista :: String, -- Motorista,
    passageiros :: [String], -- [Passageiro],
    valor :: Double,
    avaliacaoMotorista :: Int,
    avaliacoesPassageiros :: [Int]
} deriving (Show, Eq)

instance ToField TimeOfDay where
    toField time = toField $ formatTime defaultTimeLocale "%H:%M:%S" time

instance ToField Day where
    toField day = toField $ formatTime defaultTimeLocale "%Y-%m-%d" day

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

-- infoCarona :: Int -> IO String
-- infoCarona id = do
--     csvData <- readFile "~/batvolta-hs/database/Caronas.csv"
--     let caronas = parseCsv csvData
--     let maybeCarona = find (\c -> caronaId c == id) caronas
--     case maybeCarona of
--         Just carona -> return (informacoesSobreCarona carona)
--         Nothing -> return "Carona not found"

criarCarona :: TimeOfDay -> Day -> String -> String -> String -> Double -> IO ()
criarCarona hora date origem destino motorista valor = do
    let carona = Carona {
        cid = 0,
        hora = hora,
        date = date,
        origem = origem,
        destino = destino,
        motorista = motorista,
        passageiros = [],
        valor = valor,
        avaliacaoMotorista = -1,
        avaliacoesPassageiros = [-1]
    }
    writeArquivoCarona carona
    putStrLn "Carona criada com sucesso!"


getCaronasList :: IO [Carona]
getCaronasList = Csv.get parseCarona "./database/Caronas.csv"


apagarCarona :: Int -> IO ()
apagarCarona cidToDelete = do
    putStrLn "here"
    deleteCaronaById cidToDelete
    putStrLn "here"


deleteCaronaById :: Int -> IO ()
deleteCaronaById cidToDelete = do
    let csvPath = "./database/Caronas.csv"
    delete (\c -> cid c == cidToDelete) parseCarona csvPath
    putStrLn "Carona deletada com sucesso!"


parseCarona :: String -> Carona
parseCarona line = case splitOn "," line of
    [cidStr, horaStr, dateStr, origem, destino, motorista, passageirosStr, valorStr, avaliacaoMotoristaStr, avaliacoesPassageirosStr] ->
        Carona {
            cid = readInt cidStr,
            hora = parseTimeOrError True defaultTimeLocale "%H:%M:%S" horaStr,
            date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateStr,
            origem = origem,
            destino = destino,
            motorista = motorista,
            passageiros = splitOn ";" passageirosStr,
            valor = readDouble valorStr,
            avaliacaoMotorista = readInt avaliacaoMotoristaStr,
            avaliacoesPassageiros = map readInt (splitOn ";" avaliacoesPassageirosStr)
        }
    _ -> error "Invalid line format for Carona"

-- Helper function to safely parse Double from String
readDouble :: String -> Double
readDouble = fromMaybe 0.0 . fmap read . nonEmpty

-- Helper function to safely parse Int from String
readInt :: String -> Int
readInt = fromMaybe 0 . fmap read . nonEmpty

-- Helper function to filter out empty strings and convert to Maybe
nonEmpty :: String -> Maybe String
nonEmpty "" = Nothing
nonEmpty s  = Just s

writeArquivoCarona :: Carona -> IO ()
writeArquivoCarona carona = do
    let fileName = "./database/Caronas.csv"
    arq <- openFile fileName AppendMode
    BL.hPutStr arq $ encode [carona]
    hClose arq

-- Criar o arquivo CSV da Carona (O Main iniciaria todos os arquivos CSV ao invés de estarem criados já)
-- criarArquivoCSV :: String
-- criarArquivoCSV = do
--     -- Cabeçalho como um ByteString
--     let header = B8.pack "Data,Hora,Disponibilidade,Responsavel,ListaEspera\n"
