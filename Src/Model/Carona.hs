module Src.Model.Carona where

import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Calendar (Day)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.List (intercalate)
import Numeric (showFFloat)

data Carona = Carona
  { cid :: Int,
    hora :: TimeOfDay,
    date :: Day,
    origem :: String,
    destino :: String,
    motorista :: String,
    passageiros :: [String],
    valor :: Double,
    avaliacaoMotorista :: Int,
    avaliacoesPassageiros :: [Int]
  }
  deriving (Show, Eq)

instance Read Carona where
  readsPrec _ str = [(strToCarona str, "")]

-- Function to convert a Carona to a string
caronaToStr :: Carona -> String
caronaToStr (Carona c h d o dest m ps v am aps) =
  show c ++ "," ++
  show h ++ "," ++
  show d ++ "," ++
  o ++ "," ++
  dest ++ "," ++
  m ++ "," ++
  intercalate ";" ps ++ "," ++
  formatDecimal v ++ "," ++
  show am ++ "," ++
  intercalate ";" (map show aps)
  where
    formatDecimal :: Double -> String
    formatDecimal x = showFFloat (Just 2) x ""

strToCarona :: String -> Carona
strToCarona str =
  let parts = splitOn "," (filter (\c -> c /= '\r' && c /= '\\') str) in
  -- traceShow parts $
  case parts of
    [c, h, d, o, dest, m, ps, v, am, aps] ->
      Carona
        { cid = read c,
          hora = read h,
          date = read d,
          origem = o,
          destino = dest,
          motorista = m,
          passageiros = splitOn ";" ps,
          valor = read v,
          avaliacaoMotorista = read am,
          avaliacoesPassageiros = map read (splitOn ";" aps)
        }
    _ -> error "Invalid input format for Carona string"