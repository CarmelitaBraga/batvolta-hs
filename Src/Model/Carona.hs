module Src.Model.Carona where

import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Calendar (Day)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.List (intercalate)
import Numeric (showFFloat)

-- Definição do Enum para o status da carona
data StatusCarona = NaoIniciada | EmAndamento | Finalizada
  deriving (Show, Eq, Read)

data Carona = Carona
  { cid :: Int,
    hora :: TimeOfDay,
    date :: Day,
    destinos :: [String],
    motorista :: String,
    passageiros :: [String],
    valor :: Double,
    status :: StatusCarona,
    numPassageirosMaximos :: Int
  }
  deriving (Show, Eq)

instance Read Carona where
  readsPrec _ str = [(strToCarona str, "")]

-- Function to convert a Carona to a string
caronaToStr :: Carona -> String
caronaToStr (Carona c h d dest m ps v st numps) =
  show c ++ "," ++
  show h ++ "," ++
  show d ++ "," ++
  intercalate ";" dest ++ "," ++
  m ++ "," ++
  intercalate ";" ps ++ "," ++
  formatDecimal v ++ "," ++
  show st ++ "," ++
  show numps ++ ","
  where
    formatDecimal :: Double -> String
    formatDecimal x = showFFloat (Just 2) x ""

strToCarona :: String -> Carona
strToCarona str =
  let cleanedStr = reverse $ dropWhile (== ',') $ reverse str
      parts = splitOn "," cleanedStr
  in case parts of
    [c, h, d, dest, m, ps, v, st, numps] ->
      Carona
        { cid = read c,
          hora = read h,
          date = read d,
          destinos = splitOn ";" dest,
          motorista = m,
          passageiros = splitOn ";" ps,
          valor = read v,
          status = read st,
          numPassageirosMaximos = read numps
        }
    _ -> error "Invalid input format for Carona string"
