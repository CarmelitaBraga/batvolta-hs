module Main where

import Src.CLI.MotoristaCLI(menuPrincipal)
import Src.CLI.PassageiroCLI(menuPrincipalPassageiro)

main :: IO ()
main = do
    putStrLn "Bem-vindo ao Batvolta!"
    putStrLn "Escolha o tipo de usuário:"
    putStrLn "1 - Motorista"
    putStrLn "2 - Passageiro"
    putStrLn "3 - Dashboard"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> menuPrincipal
        "2" -> menuPrincipalPassageiro
        "0" -> putStrLn "Saindo do sistema..."
        _   -> do
            putStrLn "Opção inválida!"
            main
