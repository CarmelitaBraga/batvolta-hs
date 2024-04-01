module Src.Util.Utils where

import Src.Model.Carona
import Src.Model.PassageiroViagem
import Text.Regex.Posix
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar (Day)
import Data.Time.Clock
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Char
import System.IO
import Data.IORef

validarCPF :: String -> Bool
validarCPF cpf
    | length cpf /= 11 = True
    | not (all isDigit cpf) = True
    | otherwise = False

nullOrEmpty :: String -> Bool
nullOrEmpty str = null (dropWhile (== ' ') str)

-- Função para validar um email sem usar expressões regulares
validarEmail :: String -> Bool
validarEmail email = not (email =~ "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")

-- Função auxiliar para dividir uma string em partes com base em um caractere
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> splitOn delim xs

generos :: [String]
generos = ["f","m","nb"]

validarGenero :: String -> Bool
validarGenero genero = map toLower genero `notElem` generos

regioesBrasil :: [String]
regioesBrasil = ["norte", "nordeste", "centro-oeste", "sudeste", "sul"]

validaRegiao :: String -> Bool
validaRegiao regiao = map toLower regiao `notElem` regioesBrasil

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
    | toLowerCase h == toLowerCase comeco = criaSubListaFim t fim [h]
    | otherwise = retornaSubLista t comeco fim
    where 
        criaSubListaFim :: [String] -> String -> [String] -> [String]
        criaSubListaFim [] _ _ = []
        criaSubListaFim (h:t) fim subLista
            | toLowerCase h == toLowerCase fim = subLista ++ [h]
            | otherwise = criaSubListaFim t fim (subLista ++ [h])

toLowerCase :: String -> String
toLowerCase = map toLower

-- Funções auxiliares para interação com o usuário
inputString :: String -> IO String
inputString prompt = do
    putStr prompt
    hFlush stdout
    getLine

inputInt :: String -> IO Int
inputInt prompt = do
    putStrLn prompt
    input <- getLine
    if all isDigit input  -- Verifica se todos os caracteres da entrada são dígitos
        then return (read input)  -- Converte a entrada para Int se for válida
        else do
            putStrLn "Entrada inválida! Tente novamente."
            inputInt prompt

inputDouble :: String -> IO Double
inputDouble prompt = do
    putStrLn prompt
    input <- getLine
    if all isDigit input || (not (null input) && length (filter (`elem` ".") input) == 1 && all (\c -> isDigit c || c == '.') input)
        then return (read input)
        else do
            putStrLn "Entrada inválida! Tente novamente."
            inputDouble prompt

inputBoolean :: String -> IO Bool
inputBoolean prompt = do
    putStrLn prompt
    input <- map toLower <$> getLine
    if input `elem` ["aceitar", "aceito", "aceita", "sim", "s", "yes", "y"]
        then return True
    else if input `elem` ["recusar", "recuso", "rejeitar", "rejeito", "não", "nao", "n", "no"]
        then return False
    else do
            putStrLn "Entrada inválida! Tente novamente."
            inputBoolean prompt