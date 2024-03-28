
module Main where

import Src.Model.MotoristaModel(Motorista)
import Src.CLI.MotoristaCLI(menuPrincipal)
import Src.Schemas.Notificacao(insereNotificacao)
import Src.Model.NotificacaoModel(Notificacao)

main :: IO (Maybe Motorista)
main = do
    --SCHEMAS
    -- cadastraMotorista "G" "afdskj" "a" "h" "a" "a" "l"
    -- getBy "cpf" "ian"
    -- removerMotorista "afdskj"
    -- atualizarMotorista "cpfh" "57" "96"


    -- Controller
    -- realizarCadastroMotorista "11120945612" "f122" "f122" "usuario@example.com" "123231321" "123123" "123"
    -- visualizarInfoCadastroMotorista "11111111111"
    -- cancelarCadastroMotorista "ian1"
    -- atualizarCadastroMotorista "11111111111" "Cep" "Kf"
    -- realizarLoginMotorista "iangmai.com" "13"
    -- insereNotificacao 1 "Ian solicitou entrar na sua carona do dia 21, até o caja"

    menuPrincipal

    
    

    