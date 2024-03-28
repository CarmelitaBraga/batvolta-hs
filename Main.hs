module Main where

import Src.Model.MotoristaModel(Motorista)
import Src.CLI.MotoristaCLI(menuPrincipal)
import Src.Schemas.Notificacao(Notificacao(..), insereNotificacao)
main :: IO ()
main = do

          CLI.menuPrincipalPassageiro
          {- C.realizarCadastroPassageiro "Caique" "10767287426" "caique.campelo@ccc.ufcg.edu.br" "(83) 99690-0011" "58428-830" "123456"
          C.realizarCadastroPassageiro "Filipe" "11111111111" "testaaaa@gmail.com" "telefone aqui" "cep aqui" "senha aqui"
          C.atualizarCadastroPassageiro "11111111111" "Telefone" "9999999999"
          C.cancelarCadastroPassageiro "10767287426" -}
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
    let notificacao = Notificacao  "teste2" "teste1"
    insereNotificacao notificacao

    -- menuPrincipa

