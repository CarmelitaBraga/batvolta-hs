module Src.Util.ValidationCarona (
    validateCarona
) where

import Src.Model.Carona

validateCarona::Double->[String]->Int->String->String
validateCarona valor destinos limitePss motorista
    | null destinos = "Destinos deve ser uma lista com, ao menos, um valor."
    | valor <= 0 = "Valor deve ser maior que 0."
    | null motorista = "O id de Motorista noo pode ser nulo."
    | limitePss <= 0 = "O limite de passageiros deve ser maior que 0."
    | otherwise = "OK"