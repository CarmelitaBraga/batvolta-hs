module Src.Schema.CaronaSchema (
    criarCarona
) where

import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Csv (ToRecord, ToField, toRecord, record, toField, encode)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import System.IO (openFile, readFile, hClose, IOMode(AppendMode))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Carona = Carona {
    cid :: Int,
    hora :: TimeOfDay,
    date :: Day,
    origem :: String,
    destino :: String,
    motorista :: String, -- Motorista,
    passageiros :: [String], -- Passageiro],
    valor :: Double,
    avaliacaoMotorista :: Int,
    avaliacoesPassageiros :: [Int]
} deriving (Show)

-- instance ToRecord Carona where
--     toRecord c = record [
--         toField (cid c),
--         toField (hora c),
--         toField (date c),
--         toField (origem c),
--         toField (destino c),
--         toField (motorista c),
--         toField (passageiros c),
--         toField (valor c),
--         toField (avaliacaoMotorista c),
--         toField (avaliacoesPassageiros c)
--     ]

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


