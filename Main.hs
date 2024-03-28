module Main where

import Src.Model.MotoristaModel(Motorista)
import Src.CLI.PassageiroCLI(menuPrincipalPassageiro)
import Src.Schemas.SchemaPassageiro(Passageiro)

main :: IO ()
main = do
      menuPrincipalPassageiro
      print "Bem vindo ao sistema de transporte!"


