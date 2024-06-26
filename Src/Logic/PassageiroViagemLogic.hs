module Src.Logic.PassageiroViagemLogic  where

import Src.Model.PassageiroViagem
import Src.Schemas.PassageiroViagemSchema
import Src.Schemas.CaronaSchema
import Data.List
import Data.Function (on)
import Data.Ord (comparing)
import Src.Model.Carona (Carona(status, motorista))
import Src.Schemas.Notificacao


infoViagem :: Int -> IO String
infoViagem viagemId = do
    viagens <- getAllViagens  -- Assuming you have a function getAllViagens to get all viagens
    let maybeViagem = find (\v -> viagemId == pid v) viagens
    case maybeViagem of
        Nothing -> return "Viagem not found"
        Just viagem -> return $
            "Id: " ++ show (pid viagem) ++
            ", Carona Id: " ++ show (cId viagem) ++
            ", Aceita: " ++ show (aceita viagem) ++
            ", caminho: [" ++ intercalate ", " (caminho viagem) ++ "]" ++
            ", Avaliacao Motorista: " ++ show (avaliacaoMtrst viagem) ++
            ", Passageiro: " ++ passageiroId viagem

alterarStatusViagem::String->Int->String->IO String
alterarStatusViagem idPassageiro idCarona resp = do
    maybeCarona <- getCaronaById [idCarona]
    if null maybeCarona then
        return "Nenhuma carona encontrada com este id."
    else updateSolicitacaoViagem idCarona idPassageiro resp

avaliaMotorista::Int->String->Int->IO String
avaliaMotorista idCarona idPassageiro aval = if aval <= 0 || aval > 5 then
    return "Valor invalido!"
else do
    updateAvaliacaoViagem idCarona idPassageiro aval
    return "Motorista avaliado com sucesso!"

solicitaParticiparCarona :: Int -> String -> String -> String -> IO String
solicitaParticiparCarona idCarona idPassageiro origem destino = do
    -- checar se tem rota
    -- checar se tem lugar
    -- checar se tem carona
    -- checar se o passageiro ja esta
    maybeCarona <- getCaronaById [idCarona]
    if null maybeCarona
        then return "Carona inexistente!"
        else do
            let carona = head maybeCarona
                rota = getCaminho carona origem destino
            if null rota
                then return "Essa carona não possui essa rota!"
                else do
                    let mensagem = "O Passageiro: " ++ idPassageiro ++ " solicitou entrar na corrida de id: " ++ show idCarona
                    insereNotificacao (motorista carona) idPassageiro idCarona mensagem
                    criarViagemPassageiro idCarona False (getCaminho carona origem destino) 0 idPassageiro
                    return "Registro de Passageiro em Carona criado com sucesso!"

infoTrechoByCaronaPassageiro :: Int -> String -> IO String
infoTrechoByCaronaPassageiro idCarona idPassageiro = do
    maybeViagem <- getViagemByCaronaPassageiro idCarona idPassageiro
    if null maybeViagem
        then return "Trecho de carona inexistente para o passageiro informado!"
        else do
            let viagem = head maybeViagem
                viagemId = pid viagem
            infoViagem viagemId

cancelaViagemPassageiro :: Int -> String -> IO String
cancelaViagemPassageiro idCarona idPassageiro = do
    maybeViagem <- getViagemByCaronaPassageiro idCarona idPassageiro
    case maybeViagem of
        [] -> return "Trecho de carona inexistente para o passageiro informado!"
        [viagem] -> do
            let viagemId = pid viagem
            if aceita viagem then
                return "O passageiro ja foi aceito, não podera mais cancelar."
            else do
                deleteViagemById viagemId
                return "Carona cancelada com sucesso!"

possuiPassageiroViagemFalse :: Int -> Int -> IO Bool
possuiPassageiroViagemFalse idCarona idPassageiroViagem = do
    passageiroViagem <- getViagemById [idPassageiroViagem]
    return (cId (head passageiroViagem) == idCarona)

infoViagemByPassageiro::String-> IO [String]
infoViagemByPassageiro pId = do
    viagens <- getViagemByColumn "passageiroId" pId
    mapM (infoViagem . pid) viagens

infoPassageiroViagemFalseByCarona :: Int -> IO [String]
infoPassageiroViagemFalseByCarona carona = do
    carona <- getCaronaById [carona]
    selectedPassageiros <- getPassageirosViagemFalse (head carona)
    mapM (infoViagem . pid) selectedPassageiros
    
getCaronasSemAvaliacao :: String -> IO [String]
getCaronasSemAvaliacao cpf = do
    viagens <- getViagemByColumn "avaliacao" "0"
    let filtrados = filter(\v -> passageiroId v == cpf) viagens
    mapM (infoViagem . pid) filtrados

getLugaresMaisVisitados :: IO [String]
getLugaresMaisVisitados = do
    viagens <- getAllViagens
    let destinosFinais = map (\viagem -> last (caminho viagem)) viagens
        contagemDeDestinos = contarOcorrencias destinosFinais
        destinosMaisVisitados = retornaCincoMaiores contagemDeDestinos
    return destinosMaisVisitados

-- Função para contar as ocorrências de cada elemento em uma lista
contarOcorrencias :: (Eq a) => [a] -> [(a, Int)]
contarOcorrencias [] = []
contarOcorrencias (x:xs) = (x, 1 + length (filter (== x) xs)) : contarOcorrencias (filter (/= x) xs)

retornaCincoMaiores :: [(a, Int)] -> [a]
retornaCincoMaiores tupla = map fst (take 5 $ sortBy (flip $ comparing snd) tupla)