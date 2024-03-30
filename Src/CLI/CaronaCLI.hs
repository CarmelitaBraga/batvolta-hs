module Src.CLI.CaronaCLI (
    menuPrincipalCaronaMotorista,
) where

import Src.Controller.ControllerCarona as CONTROLLER
import System.IO
import Src.Model.PassageiroViagem (PassageiroViagem(cId))
import Data.IORef
import Src.Model.MotoristaModel(Motorista, getCpf)
import Src.Model.Carona (Carona(motorista))
import qualified Src.Schemas.PassageiroViagemSchema as CONTROLLER


type MotoristaRef = IORef (Maybe Motorista)

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
menuPrincipalCaronaMotorista :: MotoristaRef -> IO ()
menuPrincipalCaronaMotorista motoristaRef = do
    putStrLn "\nSelecione uma opção:"
    putStrLn "1 - Criar uma Carona"
    putStrLn "2 - Iniciar Carona"
    putStrLn "3 - Finalizar Carona"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> menuCriarCarona motoristaRef
        "2" -> menuIniciarCarona motoristaRef
        "0" -> do
            putStrLn "Saindo..."
        _   -> do
            putStrLn "Opção inválida!"
            menuPrincipalCaronaMotorista motoristaRef

menuCriarCarona :: MotoristaRef -> IO ()
menuCriarCarona motoristaRef = do
    putStrLn "\nCriar uma Carona"
    hora <- inputString "Digite a hora (no formato HH:MM): "
    date <- inputString "Digite a data (no formato DD/MM/AA): "
    origem <- inputString "Digite a origem da viagem: "
    destinos <- pedirDestinos
    valor <- inputDouble "Digite o valor: "
    numPassageirosMaximos <- inputInt "Digite a quantidade máximas de passageiros: "
    motoristaMaybe <- readIORef motoristaRef
    let motorista = getCpf motoristaMaybe
    CONTROLLER.criarCaronaMotorista hora date (origem:destinos) motorista valor numPassageirosMaximos
    menuPrincipalCaronaMotorista motoristaRef

pedirDestinos :: IO [String]
pedirDestinos = menuPedirDestinos []

menuPedirDestinos :: [String] -> IO [String]
menuPedirDestinos destinos = do
    maybeDestino <- inputString $ "Digite a " ++ show (length destinos + 1) ++ "ª cidade (aperte apenas enter para terminar de inserir destinos): "
    if null maybeDestino
        then return destinos
        else menuPedirDestinos (destinos ++ [maybeDestino])


menuIniciarCarona :: MotoristaRef -> IO ()
menuIniciarCarona motoristaRef = do
    motoristaMaybe <- readIORef motoristaRef
    let motorista = getCpf motoristaMaybe
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
    menuPrincipalCaronaMotorista motoristaRef