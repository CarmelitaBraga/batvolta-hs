module Src.Logic.PassageiroViagemLogic (
    alterarStatusViagem,
    avaliaMotorista,
    solicitaParticiparCarona,
    infoTrechoByCaronaPassageiro,
    cancelaViagemPassageiro
) where

import Src.Model.PassageiroViagem
import Src.Schema.PassageiroViagemSchema
import Src.Schema.CaronaSchema
import Src.Logic.CaronaLogic (existeRota, lugaresDisponiveis)
import Data.List (find, intercalate)

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
            ", Avaliação Motorista: " ++ show (avaliacaoMtrst viagem) ++
            ", Passageiro: " ++ passageiroId viagem

alterarStatusViagem::String->Int->String->IO String
alterarStatusViagem idPassageiro idCarona resp = do
    maybeCarona <- getCaronaById [idCarona]
    if null maybeCarona then
        return "Nenhuma carona encontrada com este id."
    else updateSolicitacaoViagem idCarona idPassageiro resp

avaliaMotorista::Int->String->Int->IO String
avaliaMotorista idCarona idPassageiro aval = do
    if aval <= 0 || aval > 5 then
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
            info <- infoViagem viagemId
            return info

cancelaViagemPassageiro :: Int -> String -> IO String
cancelaViagemPassageiro idCarona idPassageiro = do
    maybeViagem <- getViagemByCaronaPassageiro idCarona idPassageiro
    if null maybeViagem
        then return "Trecho de carona inexistente para o passageiro informado!"
        else do
            let viagem = head maybeViagem
                viagemId = pid viagem
            deleteViagemById viagemId
            -- TODO: checar se a carona ja foi inicializada e se passageiro ta no carro
            return "Carona cancelada com sucesso!"
