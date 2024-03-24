
module Main where

import Src.Schemas.Motorista(Motorista, cadastraMotorista, getBy, removerMotorista, atualizarMotorista)

main :: IO (Maybe Motorista)
main = do
    cadastraMotorista "ian" "afdskj" "a" "h" "a" "a" "l"
    -- getBy "cpf" "cpf"
    -- removerMotorista "cpf" "b"
    -- atualizarMotorista "cpf" "a" "57"




    

    