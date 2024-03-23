module Main where
import Data.Csv
import Src.Schemas.Passageiros

main :: IO ()
main = do
     cadastraPassageiro  "Jão" "111.111.222-66" "jão@gmail.com" "9999-9999" "12345-678" "senha123"
    
