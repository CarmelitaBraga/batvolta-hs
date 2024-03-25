module Src.Schema.CaronaSchema (
    criarCarona, apagarCarona, getCaronaById, getAllCaronas
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

getAllCaronas :: IO [Carona]
getAllCaronas = Csv.get strToCarona "./database/Caronas.csv"

getCaronaById :: [Int] -> IO [Carona]
getCaronaById targets = do
  caronasList <- Csv.get strToCarona "./database/Caronas.csv"
  let result = filter (\u -> cid u `elem` targets) caronasList
  return result

apagarCarona :: Int -> IO ()
apagarCarona cidToDelete = deleteCaronaById cidToDelete

deleteCaronaById :: Int -> IO ()
deleteCaronaById cidToDelete = do
    let csvPath = "./database/Caronas.csv"
    delete (\c -> cid c == cidToDelete) strToCarona caronaToStr csvPath
    putStrLn "Carona deletada com sucesso!"

-- Parse a line from CSV into a Carona
parseCarona :: String -> Carona
parseCarona line = case splitOn "," line of
    [cidStr, horaStr, dateStr, origem, destino, motorista, passageirosStr, valorStr, avaliacaoMotoristaStr, avaliacoesPassageirosStr] ->
        Carona {
            cid = read cidStr,
            hora = parseTimeOrError True defaultTimeLocale "%H:%M" horaStr,
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
