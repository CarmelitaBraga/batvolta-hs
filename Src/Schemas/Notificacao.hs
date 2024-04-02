module Src.Schemas.Notificacao where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.List
import System.IO
    ( hIsEOF, withFile, IOMode(WriteMode, AppendMode, ReadMode) )
import Data.Csv
import Control.Monad (MonadPlus(mzero))
import qualified Data.Vector as V
import Src.Model.NotificacaoModel(Notificacao(..))
import Src.Model.MotoristaModel (Motorista(cpf))

type CounterState = Int 

csvPath :: FilePath
csvPath = "./database/notificacaoMotorista.csv"
csvPathPassageiro = "./database/notificacaoPassageiro.csv"

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



insereNotificacao :: String -> String -> Int -> String -> IO (Maybe Notificacao)
insereNotificacao idMotorista idPassageiro idCarona conteudoCriar = do
    isEmpty <- checkIsEmpty csvPath
    if isEmpty
        then do
            let nextId = 0
                notificacao = Notificacao nextId idMotorista idPassageiro idCarona conteudoCriar 
                csvData = encode [notificacao]
                header = B8.pack "idNotificacao,idMotorista,idPassageiro,idCarona,conteudo\n"
                final = BL.fromStrict header <> csvData
            withFile csvPath WriteMode $ \handle -> do
                BL.hPutStr handle final
            return notificacao
       else do
            nextId <- incrementCounter counterState
            let notificacao = Notificacao nextId idMotorista idPassageiro idCarona conteudoCriar
                csvData = encode [notificacao]
            withFile csvPath AppendMode $ \handle -> do
                BL.hPutStr handle csvData
            return notificacao
    return Nothing

insereNotificacaoPassageiro :: String -> String -> Int -> String -> IO (Maybe Notificacao)
insereNotificacaoPassageiro idMotorista idPassageiro idCarona conteudoCriar = do
    isEmpty <- checkIsEmpty csvPathPassageiro
    if isEmpty
        then do
            let nextId = 0
                notificacao = Notificacao nextId idMotorista idPassageiro idCarona conteudoCriar 
                csvData = encode [notificacao]
                header = B8.pack "idNotificacao,idMotorista,idPassageiro,idCarona,conteudo\n"
                final = BL.fromStrict header <> csvData
            withFile csvPathPassageiro WriteMode $ \handle -> do
                BL.hPutStr handle final
            return notificacao
       else do
            nextId <- incrementCounter counterState
            let notificacao = Notificacao nextId idMotorista idPassageiro idCarona conteudoCriar
                csvData = encode [notificacao]
            withFile csvPathPassageiro AppendMode $ \handle -> do
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

getBy :: String -> IO [Notificacao]
getBy atributoDesejado = do
    notificacoes <- carregarNotificacoes csvPath
    let notificacoesEncontradas = filter (\n -> idMotorista n == atributoDesejado) notificacoes
        notificacoesOrdenadas = sortBy (\n1 n2 -> compare (idNotificacao n2) (idNotificacao n1)) notificacoesEncontradas
    return notificacoesOrdenadas

passageiroGetBy :: String -> IO [Notificacao]
passageiroGetBy atributoDesejado = do
    notificacoes <- carregarNotificacoes csvPathPassageiro
    let notificacoesEncontradas = filter (\n -> idPassageiro n == atributoDesejado) notificacoes
        notificacoesOrdenadas = sortBy (\n1 n2 -> compare (idNotificacao n2) (idNotificacao n1)) notificacoesEncontradas
    return notificacoesOrdenadas


