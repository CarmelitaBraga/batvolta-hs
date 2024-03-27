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

data Notificacao = Notificacao{
    motorista :: String
    , conteudo :: String
} deriving(Show)

instance FromRecord Notificacao where
    parseRecord v
        | length v == 2 = Notificacao
            <$> v .! 0
            <*> v .! 1
        | otherwise = mzero

instance ToRecord Notificacao where
    toRecord (Notificacao conteudo) = record
        [
            toField motorista
            , toField conteudo
        ]

csvPath :: FilePath
csvPath = "./database/notificacao.csv"


insereNotificacao :: Notificacao -> IO ()
insereNotificacao notificacao = do
    isEmpty <- checkIsEmpty csvPath
    if isEmpty
        then do
            let csvData = encode [notificacao]
                header = "cpf_carona,cpf_motorista,conteudo\n"
                final = header ++ T.unpack csvData
            TIO.writeFile csvPath final
            putStrLn "Notificação inserida com sucesso"
        else do
            let csvData = encode [notificacao]
            currentData <- TIO.readFile csvPath
            let (headerLine : existingContent) = T.lines currentData
                newData = T.unlines (headerLine : T.unpack csvData : existingContent)
            TIO.writeFile csvPath newData
            putStrLn "Notificação inserida com sucesso"



checkIsEmpty :: FilePath -> IO Bool
checkIsEmpty path = do
    withFile path ReadMode $ \handle -> do
        hIsEOF handle

