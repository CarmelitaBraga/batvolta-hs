module Src.Logic.PassageiroViagemLogic (
    alterarStatusViagem,
    avaliaMotorista,
    solicitaParticiparCarona,
    infoTrechoByCaronaPassageiro
) where

import Src.Model.PassageiroViagem
import Src.Schema.PassageiroViagemSchema
import Src.Schema.CaronaSchema
import Src.Logic.CaronaLogic (existeRota, lugaresDisponiveis)
import Data.List (find)

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
            ", Origem: " ++ origemPass viagem ++
            ", Destino: " ++ destino viagem ++
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

solicitaParticiparCarona::Int->String->String->String->IO String
solicitaParticiparCarona idCarona idPassageiro ori dest = do
    -- checar se tem rota
    -- checar se tem lugar
    -- checar se tem carona
    -- checar se o passageiro ja esta
    maybeCarona <- getCaronaById [idCarona]
    if null maybeCarona
        then return "Carona inexistente!"
        else do
            let carona = head maybeCarona
            if existeRota carona ori dest && lugaresDisponiveis carona
                then do
                    criarViagemPassageiro idCarona False ori dest 0 idPassageiro
                    return "Registro de Passageiro em Carona criado com sucesso!"
                else return "Registro de Passageiro em Carona indisponivel no momento!"

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
