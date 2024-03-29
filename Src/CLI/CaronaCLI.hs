module Src.CLI.CaronaCLI (
    menuPrincipal
) where

import Src.Controller.ControllerCarona as CONTROLLER
import System.IO
import Src.Model.PassageiroViagem (PassageiroViagem(cId))

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
menuPrincipal :: String -> IO ()
menuPrincipal motorista = do
    putStrLn "\nSelecione uma opção:"
    putStrLn "1 - Criar uma Carona"
    putStrLn "2 - Iniciar Carona"
    putStrLn "2 - Finalizar Carona"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> menuCriarCarona motorista
        "2" -> menuIniciarCarona motorista
        "0" -> do
            putStrLn "Saindo..."
        _   -> do
            putStrLn "Opção inválida!"
            menuPrincipal motorista

menuCriarCarona :: String -> IO ()
menuCriarCarona motorista = do
    putStrLn "\nCriar uma Carona"
    hora <- inputString "Digite a hora (no formato HH:MM): "
    date <- inputString "Digite a data (no formato DD/MM/AA): "
    origem <- inputString "Digite a origem da viagem: "
    destinos <- pedirDestinos
    valor <- inputDouble "Digite o valor: "
    numPassageirosMaximos <- inputInt "Digite a quantidade máximas de passageiros: "
    
    CONTROLLER.criarCaronaMotorista hora date origem destinos motorista valor numPassageirosMaximos

pedirDestinos :: IO [String]
pedirDestinos = menuPedirDestinos []

menuPedirDestinos :: [String] -> IO [String]
menuPedirDestinos destinos = do
    maybeDestino <- inputString $ "Digite a " ++ show (length destinos + 1) ++ "ª cidade (aperte apenas enter para terminar de inserir destinos): "
    if null maybeDestino
        then return destinos
        else menuPedirDestinos (destinos ++ [maybeDestino])

menuIniciarCarona :: String -> IO ()
menuIniciarCarona motorista = do
    possuiCarona <- CONTROLLER.possuiCaronaNaoIniciadaController motorista
    if possuiCarona then do
        putStrLn "Qual carona você deseja iniciar:"
        caronas <- CONTROLLER.infoCaronasNaoIniciadas motorista
        putStrLn caronas
        cId <- inputInt "Digite o Id da carona: "
        iniciarCarona <- CONTROLLER.inicializarCaronaStatus cId
        putStrLn iniciarCarona
    else do
        putStrLn "Não existem caronas possíveis de se iniciar!"