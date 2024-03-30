module Src.Util.Utils (retornaSubLista, getCaronaAttribute, splitBy, validarData, validarHorario, stringToTimeOfDay, stringToDay, getViagemAttribute) where

import Src.Model.Carona
import Src.Model.PassageiroViagem
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
    | attr == "destinos" = unwords (destinos carona)
    | attr == "motorista" = motorista carona
    | attr == "passageiros" = unwords (passageiros carona)
    | attr == "valor" = show (valor carona)
    | attr == "status" = show (status carona)
    | attr == "numPassageirosMaximos" = show (numPassageirosMaximos carona)
    | otherwise = error "Invalid attribute"
    

getViagemAttribute :: PassageiroViagem -> String -> String
getViagemAttribute viagem attr
    | attr == "pid" = show (pid viagem)
    | attr == "cid" = show (cId viagem)
    | attr == "aceita" = show (aceita viagem)
    | attr == "caminho" = unwords (caminho viagem)
    | attr == "avaliacao" = show (avaliacaoMtrst viagem)
    | attr == "passageiroId" = passageiroId viagem
    | otherwise = error "Invalid attribute"

retornaSubLista :: [String] -> String -> String -> [String]
retornaSubLista [] _ _ = []
retornaSubLista (h:t) comeco fim
    | h == comeco = criaSubListaFim t fim [h]
    | otherwise = retornaSubLista t comeco fim

criaSubListaFim :: [String] -> String -> [String] -> [String]
criaSubListaFim [] _ _ = []
criaSubListaFim (h:t) fim subLista
    | h == fim = subLista ++ [h]
    | otherwise = criaSubListaFim t fim (subLista ++ [h])