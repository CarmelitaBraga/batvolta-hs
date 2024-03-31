module Src.Model.MotoristaModel(
    Motorista(..),
    confereSenha,
    toRecord,
    parseRecord,
    getCpf
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
    cnh :: String,
    genero :: String,
    regiao :: String
} deriving(Eq)


instance ToRecord Motorista where
    toRecord (Motorista cpf cep nome email telefone senha cnh genero regiao) = record
        [ toField cpf
        , toField cep
        , toField nome
        , toField email
        , toField telefone
        , toField senha
        , toField cnh
        , toField genero
        , toField regiao
        ]


instance FromRecord Motorista where
    parseRecord v
        | length v == 9 = Motorista
            <$> v .! 0
            <*> v .! 1
            <*> v .! 2
            <*> v .! 3
            <*> v .! 4
            <*> v .! 5
            <*> v .! 6
            <*> v .! 7
            <*> v .! 8
        | otherwise = mzero


--Funciona como ToString d
instance Show Motorista where
    show (Motorista cpf cep nome email telefone senha cnh genero regiao) =
        "Motorista { CPF: " ++ cpf ++
        ", CEP: " ++ cep ++
        ", Nome: " ++ nome ++
        ", E-mail: " ++ email ++
        ", Telefone: " ++ telefone ++
        ", CNH: " ++ cnh ++
        ", Genero: " ++ genero ++
        ", Regiao: " ++ regiao ++
        " }"

confereSenha :: Motorista -> String -> Bool
confereSenha motorista senhaPassada = senhaPassada == senha motorista

getCpf :: Maybe Motorista -> String
getCpf Nothing = "Motorista não encontrado"
getCpf (Just motorista) = cpf motorista