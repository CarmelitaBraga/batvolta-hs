
module Main where

import Src.Schemas.Motorista(Motorista, cadastraMotorista, getBy)

main :: IO (Maybe Motorista)
main = do
    cadastraMotorista "Teste" "Teste" "Nomek" "Email" "Telefone" "Senha" "CNH"
    getBy "cpf" "cpf"