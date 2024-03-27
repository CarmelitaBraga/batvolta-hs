module Src.Util.Utils (getCaronaAttribute, splitBy) where

import Src.Model.Carona

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

splitBy :: Char -> String -> [String]
splitBy sep = wordsWhen (== sep)

getCaronaAttribute :: Carona -> String -> String
getCaronaAttribute carona attr
    | attr == "cid" = show (cid carona)
    | attr == "hora" = show (hora carona)
    | attr == "date" = show (date carona)
    | attr == "origem" = origem carona
    | attr == "destino" = destino carona
    | attr == "motorista" = motorista carona
    | attr == "passageiros" = unwords (passageiros carona)
    | attr == "valor" = show (valor carona)
    | attr == "avaliacaoMotorista" = show (avaliacaoMotorista carona)
    | attr == "avaliacoesPassageiros" = unwords (map show (avaliacoesPassageiros carona))
    | otherwise = error "Invalid attribute"