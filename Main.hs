module Main where
     import Data.Csv
     import Src.Schemas.SchemaPassageiro as P
     import Src.Controller.ControllerPassageiro as C
     import Src.CLI.PassageiroCLI as CLI

     main :: IO (Maybe Passageiro)
     main = do
          CLI.menuPrincipalPassageiro
          {- C.realizarCadastroPassageiro "Caique" "10767287426" "caique.campelo@ccc.ufcg.edu.br" "(83) 99690-0011" "58428-830" "123456"
          C.realizarCadastroPassageiro "Filipe" "11111111111" "testaaaa@gmail.com" "telefone aqui" "cep aqui" "senha aqui"
          C.atualizarCadastroPassageiro "11111111111" "Telefone" "9999999999"
          C.cancelarCadastroPassageiro "10767287426" -}