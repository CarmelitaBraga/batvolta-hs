module Src.Model.NotificacaoModel(
    Notificacao(..),
    toRecord,
    parseRecord
    )where

import Control.Monad (MonadPlus(mzero))
import Data.Csv

data Notificacao = Notificacao{
    idNotificacao :: Int
    ,idMotorista :: String
    ,idPassageiro :: String
    ,idcarona :: Int
    ,conteudo :: String
} deriving()

instance FromRecord Notificacao where
    parseRecord v
        | length v == 5 = Notificacao
            <$> v .! 0
            <*> v .! 1
            <*> v .! 2
            <*> v .! 3
            <*> v .! 4
        | otherwise = mzero

instance ToRecord Notificacao where
    toRecord (Notificacao idNotificacao idMotorista idPassageiro idcarona conteudo) = record
        [
            toField idNotificacao
            ,toField idMotorista
            ,toField idPassageiro
            ,toField  idcarona
            , toField conteudo
            
        ]

instance Show Notificacao where
  show (Notificacao idNotif idMot idPass idCar conteudo') =
    "Info: \n idNotificacao = " ++ show idNotif ++
    ", idMotorista = " ++ show idMot ++
    ", idPassageiro = " ++ show idPass ++
    ", idCarona = " ++ show idCar ++
    ", conteudo = " ++ show conteudo' ++
    "."
