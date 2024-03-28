module Src.Schemas.Notificacao where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.IO
    ( hIsEOF, withFile, IOMode(WriteMode, AppendMode, ReadMode) )
import Data.Csv
import Control.Monad (MonadPlus(mzero))
import qualified Data.Vector as V

data Notificacao = Notificacao{
    motorista :: String
    , conteudo :: String
    ,idNotificacao :: Int
} deriving(Show)

instance FromRecord Notificacao where
    parseRecord v
        | length v == 3 = Notificacao
            <$> v .! 0
            <*> v .! 1
            <*> v .! 2
        | otherwise = mzero

instance ToRecord Notificacao where
    toRecord (Notificacao id motorista conteudo) = record
        [
            toField  conteudo
            , toField motorista
            , toField id
        ]

type CounterState = Int 

csvPath :: FilePath
csvPath = "./database/notificacaoMotorista.csv"

counterState :: CounterState
counterState = 0

incrementCounter :: CounterState -> IO CounterState
incrementCounter currentState = do
    allNotificacoes <- carregarNotificacoes csvPath
    let nextId = findNextId currentState allNotificacoes
    return nextId

findNextId :: CounterState -> [Notificacao] -> CounterState
findNextId currentId listaNotificacoes =
    if any (\u -> idNotificacao u == currentId) listaNotificacoes
        then findNextId (currentId + 1) listaNotificacoes
        else currentId



insereNotificacao :: Notificacao -> IO ()
insereNotificacao notificacao = do
    isEmpty <- checkIsEmpty csvPath
    if isEmpty
        then do
            let csvData = encode [notificacao]
                nextId = 0
                header = B8.pack "idNotificacao,motorista,conteudo\n"
                final = BL.fromStrict header <> csvData
            withFile csvPath WriteMode $ \handle -> do
                BL.hPutStr handle final
       else do
            nextId <- incrementCounter counterState
            let csvData = encode [notificacao]
            withFile csvPath AppendMode $ \handle -> do
                BL.hPutStr handle csvData


checkIsEmpty :: FilePath -> IO Bool
checkIsEmpty path = do
    withFile path ReadMode $ \handle -> do
        hIsEOF handle


carregarNotificacoes :: FilePath -> IO [Notificacao]
carregarNotificacoes path = do
    withFile path ReadMode $ \handle -> do
        csvData <- BL.hGetContents handle
        case decode NoHeader csvData of
            Left err -> do
                putStrLn $ "error: " ++ err
                return []
            Right notificacao -> do
                return $ V.toList notificacao