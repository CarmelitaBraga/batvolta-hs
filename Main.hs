module Main where
import Data.Csv
import Src.Schemas.Passageiros as P

main :: IO (Maybe Passageiro)
main = do
     P.cadastraPassageiro "Caique" "107.672.774-30" "iquecc5@gmail.com" "(82) 99690-0011" "58428-830" "123456"