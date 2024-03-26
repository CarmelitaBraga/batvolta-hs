module Main where
import Data.Csv
import Src.Schemas.SchemaPassageiro as P
import Src.Logic.LogicPassageiro as LP
import qualified Src.Schemas.SchemaPassageiro as LP

main :: IO (Maybe Passageiro)
main = do
     LP.cadastraPassageiroLogic "Caique" "10767287426" "caique.campelo@ccc.ufcg.edu.br" "(83) 99690-0011" "58428-830" "123456"
     --LP.cadastraPassageiroLogic "Filipe" "11111111111" "testaaaa@gmail.com" "telefone aqui" "cep aqui" "senha aqui"
     --LP.removePassageiroByCpfLogic "107.62.774-30"
     -- LP.removePassageiroByCpfLogic "107.672.774-30"
     LP.editPassageiroCSVLogic "10767287426" "" "" "304820"
     LP.editPassageiroCSVLogic "11111111111" "57051-230" "(82) 99690-0017" "304820"
     LP.getPassageiroByCpfLogic "11111111111"
     
