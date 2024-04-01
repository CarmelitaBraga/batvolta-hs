module Src.CLI.DashboardCLI where

import System.IO
import Data.IORef
import Src.Util.Utils
import Src.Controller.ControllerCarona

menuPrincipalDashboard :: IO ()
menuPrincipalDashboard = do
        putStrLn "\nSelecione uma opção: "
        putStrLn "1 - Top-Rated Motoristas"
        putStrLn "2 - Top-Rated Motoristas por Região"
        putStrLn "3 - Top-Rated Passageiros"
        putStrLn "4 - Destinos mais visitados"
        putStrLn "0 - Sair"
        opcao <- getLine
        case opcao of
            "1" -> imprimirMotoristasDirijoes
            "2" -> imprimirTopMotoristasPorRegiao
            -- "3" -> putStrLn imprimirTopPassageiros
            "4" -> imprimirDestinosMaisVisitados
            "0" -> putStrLn "Fim da interação!"
            _   -> do
                putStrLn "Opção inválida!"
                menuPrincipalDashboard

imprimirDestinosMaisVisitados :: IO()
imprimirDestinosMaisVisitados = do
    putStrLn "\nTOP 5 LOCAIS MAIS USADOS COMO DESTINO:"
    result <- imprimirDestinosComMaisVisitas
    putStrLn result
    menuPrincipalDashboard
    
-- imprimirTopPassageiros::IO()
-- imprimirTopPassageiros = do
--     response <- imprimirTopPassageiros
--     return response

imprimirTopMotoristasPorRegiao:: IO()
imprimirTopMotoristasPorRegiao = do
    regiao <- inputString "Digite a região: "
    response <- imprimirMotoristasPorRegiao regiao
    putStrLn response
    menuPrincipalDashboard

imprimirMotoristasDirijoes :: IO()
imprimirMotoristasDirijoes = do
    result <- imprimirMelhoresMotorista
    putStrLn result
    menuPrincipalDashboard