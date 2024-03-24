
module Main where

import Src.Schemas.Motorista(Motorista, cadastraMotorista, getBy, removerMotorista, atualizarMotorista)
import Src.Controller.CRUD_Motorista(realizarCadastroMotorista, cancelarCadastroMotorista, atualizarCadastroMotorista, visualizarInfoCadastroMotorista)

main :: IO (Maybe Motorista)
main = do
    --SCHEMAS
    -- cadastraMotorista "G" "afdskj" "a" "h" "a" "a" "l"
    -- getBy "cpf" "ian"
    -- removerMotorista "afdskj"
    -- atualizarMotorista "cpfh" "57" "96"

    -- CRUD
    -- realizarCadastroMotorista "ian" "f" "f" "f" "f" "f" "f"
    -- visualizarInfoCadastroMotorista "cpf" "ian"
    -- cancelarCadastroMotorista "ian"
    -- atualizarCadastroMotorista "ian" "cep" "50501"

