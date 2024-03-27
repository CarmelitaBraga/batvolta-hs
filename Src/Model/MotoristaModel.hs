module Src.Model.MotoristaModel(
     Motorista(..),
    confereSenha,
    toRecord,
    parseRecord
) where

import Control.Monad (MonadPlus(mzero))
import Data.Csv

data Motorista = Motorista{
    cpf :: String,
    cep :: String,
    nome :: String,
    email :: String,
    telefone :: String,
    senha :: String,
    cnh :: String
} deriving(Eq)


instance ToRecord Motorista where
    toRecord (Motorista cpf cep nome email telefone senha cnh) = record
        [ toField cpf
        , toField cep
        , toField nome
        , toField email
        , toField telefone
        , toField senha
        , toField cnh
        ]


instance FromRecord Motorista where
    parseRecord v
        | length v == 7 = Motorista
            <$> v .! 0
            <*> v .! 1
            <*> v .! 2
            <*> v .! 3
            <*> v .! 4
            <*> v .! 5
            <*> v .! 6
        | otherwise = mzero


--Funciona como ToString d
instance Show Motorista where
    show (Motorista cpf cep nome email telefone senha cnh) =
        "Motorista { CPF: " ++ cpf ++
        ", CEP: " ++ cep ++
        ", Nome: " ++ nome ++
        ", E-mail: " ++ email ++
        ", Telefone: " ++ telefone ++
        ", CNH: " ++ cnh ++
        " }"

confereSenha :: Motorista -> String -> Bool
confereSenha motorista senhaPassada = senhaPassada == senha motorista
