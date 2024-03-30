module Src.Util.ValidationCarona (
    validateCarona
) where

import Src.Model.Carona

validateCarona::Double->String->[String]->Int->String->String
validateCarona valor origem destinos limitePss motorista
    | null origem = "Origem nao pode ser nula."
    | null destinos = "Destinos deve ser uma lista com, ao menos, um valor."
    | valor <= 0 = "Valor deve ser maior que 0."
    | null motorista = "O id de Motorista noo pode ser nulo."
    | limitePss <= 0 = "O limite de passageiros deve ser maior que 0."
    | otherwise = "OK"