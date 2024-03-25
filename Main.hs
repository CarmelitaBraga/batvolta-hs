module Main where
import Data.Csv
import Src.Schemas.Passageiros as P

main :: IO (Maybe Passageiro)
main = do
     P.cadastraPassageiro "Caique" "107.672.774-30" "iquecc5@gmail.com" "(82) 99690-0011" "58428-830" "123456"
     P.cadastraPassageiro "Caique" "107.672.874-26" "caique.campelo@ccc.ufcg.edu.br" "(83) 99690-0011" "58428-830" "123456"
     P.removePassageiroByCpf "107.62.774-30"
     P.removePassageiroByCpf "107.672.774-30"
     P.editPassageiroCSV "107.672.874-26" "57051-230" "(82) 99690-0017" "304820"
     P.editPassageiroCSV "107.672.674-26" "57051-230" "(82) 99690-0017" "304820"
