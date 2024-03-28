module Src.Util.Utils (getCaronaAttribute, splitBy, validarData, validarHorario, stringToTimeOfDay, stringToDay) where

import Src.Model.Carona
import Text.Regex.Posix
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar (Day)
import Data.Time.Clock
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Função para converter String em TimeOfDay
stringToTimeOfDay :: String -> TimeOfDay
stringToTimeOfDay str = fromMaybe (error "Hora inválida.") $ parseTimeM True defaultTimeLocale "%H:%M" str

-- Função para converter String em Day
stringToDay :: String -> Day
stringToDay str = fromMaybe (error "Data inválida.") $ parseTimeM True defaultTimeLocale "%d/%m/%Y" str :: Day

validarHorario :: String -> Bool
validarHorario horario = not (horario =~ "^([01]?[0-9]|2[0-3]):[0-5][0-9]$") :: Bool

validarData :: String -> Bool
validarData date = not (date =~ "^(0[1-9]|[12][0-9]|3[01])/(0[13578]|1[02])/((202[4-9])|20[3-9][0-9]|[2-9][1-9][0-9][0-9])$" 
                || date =~ "^(0[1-9]|[12][0-9]|30)/(0[469]|11)/((202[4-9])|20[3-9][0-9]|[2-9][1-9][0-9][0-9])$" 
                || date =~ "^(0[1-9]|1[0-9]|2[0-7])/02/((202[4-9])|20[3-9][0-9]|[2-9][1-9][0-9][0-9])$") :: Bool

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