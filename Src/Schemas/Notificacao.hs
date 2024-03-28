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
import Src.Model.NotificacaoModel(Notificacao(..))

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



insereNotificacao :: Int -> String -> IO (Maybe Notificacao)
insereNotificacao idCarona conteudoCriar = do
    isEmpty <- checkIsEmpty csvPath
    if isEmpty
        then do
            let nextId = 0
                notificacao = Notificacao nextId idCarona conteudoCriar 
                csvData = encode [notificacao]
                header = B8.pack "idNotificacao,idCarona,conteudo\n"
                final = BL.fromStrict header <> csvData
            withFile csvPath WriteMode $ \handle -> do
                BL.hPutStr handle final
            return notificacao
       else do
            nextId <- incrementCounter counterState
            let notificacao = Notificacao nextId idCarona conteudoCriar
                csvData = encode [notificacao]
            withFile csvPath AppendMode $ \handle -> do
                BL.hPutStr handle csvData
            return notificacao
    return Nothing

checkIsEmpty :: FilePath -> IO Bool
checkIsEmpty path = do
    withFile path ReadMode $ \handle -> do
        hIsEOF handle


carregarNotificacoes :: FilePath -> IO [Notificacao]
carregarNotificacoes path = do
    withFile path ReadMode $ \handle -> do
        csvData <- BL.hGetContents handle
        case decode HasHeader csvData of
            Left err -> do
                putStrLn $ "error: " ++ err
                return []
            Right notificacao -> do
                return $ V.toList notificacao


