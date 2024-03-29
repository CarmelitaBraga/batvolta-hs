module Src.CLI.CaronaCLI (
    menuPrincipal
) where

import Src.Logic.CaronaLogic
import System.IO

-- Funções auxiliares para interação com o usuário
inputString :: String -> IO String
inputString prompt = do
    putStr prompt
    hFlush stdout
    getLine

inputDouble :: String -> IO Double
inputDouble prompt = do
    str <- inputString prompt
    return (read str)

inputInt :: String -> IO Int
inputInt prompt = do
    str <- inputString prompt
    return (read str)

-- Implementação dos menus
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\nSelecione uma opção:"
    putStrLn "1 - Criar uma Carona"
    putStrLn "2 - Mostrar suas caronas"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> menuCriarCarona
        "2" -> do
            putStrLn "TODO"
        "0" -> do
            putStrLn "Saindo..."
        _   -> do
            putStrLn "Opção inválida!"
            menuPrincipal

menuCriarCarona :: IO ()
menuCriarCarona = do
    putStrLn "\nCriar uma Carona"
    hora <- inputString "Digite a hora (no formato HH:MM): "
    date <- inputString "Digite a data (no formato DD/MM/AA): "
    origem <- inputString "Digite a origem da viagem: "
    destinos <- pedirDestinos
    motorista <- inputString "Digite o motorista: "-- Trocar pelo motorista salvo do loginposteriormente
    valor <- inputDouble "Digite o valor: "
    numPassageirosMaximos <- inputInt "Digite a quantidade máximas de passageiros: "
    
    gerarCarona hora date origem destinos motorista valor numPassageirosMaximos

pedirDestinos :: IO [String]
pedirDestinos = menuPedirDestinos []

menuPedirDestinos :: [String] -> IO [String]
menuPedirDestinos destinos = do
    maybeDestino <- inputString $ "Digite a " ++ show (length destinos + 1) ++ "ª cidade (aperte apenas enter para terminar de inserir destinos): "
    if null maybeDestino
        then return destinos
        else menuPedirDestinos (destinos ++ [maybeDestino])