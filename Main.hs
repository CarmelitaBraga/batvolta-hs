module Main where

import Src.CLI.MotoristaCLI(menuPrincipal)
import Src.CLI.PassageiroCLI(menuPrincipalPassageiro)
import Src.CLI.DashboardCLI(menuPrincipalDashboard)
import Control.Exception
import GHC.IO.Exception (ExitCode(ExitSuccess))
main :: IO ()
main = do
    putStrLn "\nBem-vindo ao Batvolta!"
    putStrLn "Escolha o tipo de usuário:"
    putStrLn "1 - Motorista"
    putStrLn "2 - Passageiro"
    putStrLn "3 - Dashboard"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> menuPrincipal
        "2" -> menuPrincipalPassageiro
        "3" -> menuPrincipalDashboard
        "0" -> do
            putStrLn "Saindo do sistema..."
            throwIO ExitSuccess
        _   -> do
            putStrLn "Opção inválida!"
    main
