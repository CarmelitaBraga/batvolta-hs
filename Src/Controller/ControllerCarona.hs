module Src.Controller.ControllerCarona where

import Src.Logic.CaronaLogic
import Src.Logic.PassageiroViagemLogic
import Src.Model.Carona
import Src.Util.ValidationCarona
import Control.Exception (catch)
import Data.Bool (Bool)
import Data.List (intercalate)
import Src.Schemas.PassageiroViagemSchema (criarViagemPassageiro)
import Src.Model.PassageiroViagem (PassageiroViagem(passageiroId), viagemToStr)

mostrarCaronasPassageiro::String -> IO String
mostrarCaronasPassageiro pId = do
    caronas <- infoCaronaByPassageiro pId
    return $ unlines caronas

mostrarCaronasMotorista::String->IO String
mostrarCaronasMotorista mId = do
    caronas <- infoCaronaByMotorista mId
    return $ unlines caronas

mostrarCaronaPorId::String->IO String
mostrarCaronaPorId cid = do
    carona <- infoCaronaById cid
    return $ unlines carona

deletarCaronaMotorista::String -> Int ->IO String
deletarCaronaMotorista = deletarCaronaPorId

mostrarCaronasDisponiveisOrigemDestino :: String -> String -> IO String
mostrarCaronasDisponiveisOrigemDestino origem destino = do
    caronas <- infoCaronaDisponivelOriDest origem destino
    if null caronas
        then return "Nenhuma carona disponível para esta rota no momento."
        else return (intercalate "\n" (map show caronas))

possuiCaronaNaoIniciadaController :: String -> IO Bool
possuiCaronaNaoIniciadaController = possuiCaronaNaoIniciada

possuiCaronaEmAndamentoController :: String -> IO Bool
possuiCaronaEmAndamentoController = possuiCaronaEmAndamento

possuiCaronasOrigemDestinoController :: String -> String -> IO Bool
possuiCaronasOrigemDestinoController = possuiCaronaOrigemDestino

possuiCaronasPassageirosViagemFalse :: String -> IO Bool
possuiCaronasPassageirosViagemFalse motorista = do
    caronas <- infoCaronaPassageirosViagemFalse motorista
    return (not (null caronas))

possuiPassageiroViagemFalse :: Int -> IO Bool
possuiPassageiroViagemFalse = possuiPassageiroViagemFalseByCarona

possuiPassageiroViagem :: Int -> Int -> IO Bool
possuiPassageiroViagem = possuiPassageiroByCarona

infoCaronasNaoIniciadas :: String -> IO String
infoCaronasNaoIniciadas motorista = do
    caronas <- infoCaronaNaoIniciadaByMotorista motorista
    return $ unlines caronas

infoCaronasEmAndamento :: String -> IO String
infoCaronasEmAndamento motorista = do
    caronas <- infoCaronaEmAndamentoByMotorista motorista
    return $ unlines caronas

infoCaronaPassageirosViagemFalse :: String -> IO String
infoCaronaPassageirosViagemFalse motorista = do
    caronas <- infoCaronaPassageirosViagemFalseByMotorista motorista
    return $ unlines caronas

infoPassageiroViagemFalse :: Int -> IO String
infoPassageiroViagemFalse carona = do
    passageiros <- infoPassageiroViagemFalseByCarona carona
    return $ unlines passageiros

finalizarCaronaStatus :: Int -> IO String
finalizarCaronaStatus  = finalizarCarona

inicializarCaronaStatus::Int -> IO String
inicializarCaronaStatus  = iniciarCarona

responderSolicitacaoCarona::String->Int->String->IO()
responderSolicitacaoCarona idPassageiro idCarona resp = do
    status <- alterarStatusViagem idPassageiro idCarona resp
    putStrLn status

criarCaronaMotorista :: String -> String -> [String] -> String -> Double -> Int -> IO ()
criarCaronaMotorista hora date destinos motorista valor limitePss = do
    let check = validateCarona valor destinos limitePss motorista
    if check /= "OK"
        then putStrLn $ "Erro ao criar Carona: " ++ check
        else do
            result <- gerarCarona hora date destinos motorista valor limitePss
            putStrLn result

avaliarMotoristaCarona::Int->String->Int->IO()
avaliarMotoristaCarona idCarona idPassageiro aval = do
    result <- avaliaMotorista idCarona idPassageiro aval
    putStrLn result

modificarLimitePassageiros::Int->String->Int->IO()
modificarLimitePassageiros idCarona idMotorista novoLimite = do
    result <- mudaLimitePassageirosCarona idCarona idMotorista novoLimite
    putStrLn result

embarcarPassageiro::Int->String->IO()
embarcarPassageiro idCarona idPassageiro = do
    result <- adicionarPassageiro idCarona idPassageiro
    putStrLn result

desembarcarPassageiro::Int->String->IO()
desembarcarPassageiro idCarona idPassageiro = do
    result <- removerPassageiro idCarona idPassageiro
    putStrLn result

solicitarCaronaPassageiro::Int -> String -> String -> String -> IO String
solicitarCaronaPassageiro idCarona idPassageiro origem destino = do
    solicitaParticiparCarona idCarona idPassageiro origem destino

mostrarTrechoViagemPassageiro::Int->String->IO()
mostrarTrechoViagemPassageiro idCarona idPassageiro = do
    result <- infoTrechoByCaronaPassageiro idCarona idPassageiro
    putStrLn result

cancelarCaronaPassageiro::Int->String->IO()
cancelarCaronaPassageiro idCarona idPassageiro = do
    result <- cancelaViagemPassageiro idCarona idPassageiro
    putStrLn result

motoristaPossuiCaronas :: String -> IO Bool
motoristaPossuiCaronas = motoristaPossuiCarona

aceitarOuRecusarPassageiro :: String -> Int -> Bool -> IO String
aceitarOuRecusarPassageiro = recusarOuAceitarPassageiro

checarCaronaDeMotorista::Int->String->IO Bool
checarCaronaDeMotorista idCarona idMotorista = do
    caronaPertenceMotorista idCarona idMotorista

mostrarViagemPassageiro :: String-> IO String
mostrarViagemPassageiro passageiroId = do
    viagemToStr <- infoViagemByPassageiro passageiroId
    return $ unlines viagemToStr

getViagemSemAvaliacao :: String -> IO String
getViagemSemAvaliacao cpf = do
    viagemToStr <- getCaronasSemAvaliacao cpf
    return $ unlines viagemToStr

avaliarMotorista :: Int -> String -> Int -> IO String
avaliarMotorista idCarona idPassageiro aval = do
    avaliaMotorista idCarona idPassageiro aval

avaliarCarona :: String -> Int -> Int -> IO String 
avaliarCarona idMotorista idCarona aval = do
    avaliaCarona idMotorista idCarona aval

imprimirMotoristasPorRegiao::String->IO String
imprimirMotoristasPorRegiao regiao = do
    result <- getTopMotoristasByRegiao regiao
    return $ unlines result

imprimirMelhoresMotorista :: IO String
imprimirMelhoresMotorista = do
    melhores <- getMelhoresMotoristas
    return $ unlines melhores

imprimirDestinosComMaisVisitas :: IO String
imprimirDestinosComMaisVisitas = do
    lugaresMaisVisitados <- getLugaresMaisVisitados
    return $ unlines lugaresMaisVisitados

imprimirMelhoresPassageiros :: IO String
imprimirMelhoresPassageiros = do
    melhoresPassageiros <- getMelhoresPassageiros
    return $ unlines melhoresPassageiros

caronasSemAvaliacao :: String -> IO String
caronasSemAvaliacao idMotorista = do
    caronas <- caronasSemAvaliacaoByMotorista idMotorista
    return $ unlines caronas