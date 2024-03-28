module Src.Model.NotificacaoModel(
    Notificacao(..),
    toRecord,
    parseRecord
    )where

import Control.Monad (MonadPlus(mzero))
import Data.Csv

data Notificacao = Notificacao{
    idNotificacao :: Int
    ,carona :: Int
    , conteudo :: String
} deriving()

instance FromRecord Notificacao where
    parseRecord v
        | length v == 3 = Notificacao
            <$> v .! 0
            <*> v .! 1
            <*> v .! 2
        | otherwise = mzero

instance ToRecord Notificacao where
    toRecord (Notificacao idNotificacao carona conteudo) = record
        [
            toField idNotificacao
            ,toField  carona
            , toField conteudo
            
        ]

instance Show Notificacao where
    show (Notificacao id carona conteudo) =
        "Notificacao { idNotificacao = " ++ show id
        ++ ", carona = " ++ show carona
        ++ ", conteudo = " ++ show conteudo ++ " }"
