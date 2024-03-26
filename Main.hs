
module Main where

import Src.Schemas.Motorista(Motorista, cadastraMotorista, getBy, removerMotorista, atualizarMotorista)
import Src.Controller.ControllerMotorista(realizarCadastroMotorista, cancelarCadastroMotorista, atualizarCadastroMotorista, visualizarInfoCadastroMotorista)

main :: IO (Maybe Motorista)
main = do
    --SCHEMAS
    -- cadastraMotorista "G" "afdskj" "a" "h" "a" "a" "l"
    -- getBy "cpf" "ian"
    -- removerMotorista "afdskj"
    -- atualizarMotorista "cpfh" "57" "96"

    -- CRUDddaa1
    -- realizarCadastroMotorista "11120945612" "f122" "f122" "usuario@example.com" "123231321" "123123" "123"
    -- visualizarInfoCadastroMotorista "11120945612"
    -- cancelarCadastroMotorista "ian1"
    atualizarCadastroMotorista "11120945612" "Telefone" "teste2"

