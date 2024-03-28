module Src.Model.PassageiroViagem where

import Data.List.Split (splitOn)
import Data.Csv (ToRecord, ToField, toRecord, toField, record)
import Data.ByteString.Char8 (pack)

instance ToField Bool where
    toField True = pack "True"
    toField False = pack "False"

data PassageiroViagem = PassageiroViagem {
    pid :: Int,
    cId :: Int,
    aceita :: Bool,
    origemPass :: String,
    destino :: String,
    avaliacaoMtrst :: Int,
    passageiroId :: String
} deriving (Show, Eq)

instance Read PassageiroViagem where
  readsPrec _ str = [(strToViagem str, "")]

-- Function to convert a PassageiroViagem to a string
viagemToStr :: PassageiroViagem -> String
viagemToStr (PassageiroViagem p c a o d am pid) =
  show p ++ "," ++
  show c ++ "," ++
  show a ++ "," ++
  o ++ "," ++
  d ++ "," ++
  show am ++ "," ++
  pid

strToViagem :: String -> PassageiroViagem
strToViagem str =
  let parts = splitOn "," (filter (\c -> c /= '\r' && c /= '\\') str) in
  case parts of
    [p, c, a, o, d, am, pid] ->
      PassageiroViagem
        { pid = read p,
          cId = read c,
          aceita = read a,
          origemPass = o,
          destino = d,
          avaliacaoMtrst = read am,
          passageiroId = pid
        }
    _ -> error "Invalid input format for PassageiroViagem string"

parseViagem :: String -> PassageiroViagem
parseViagem line = case splitOn "," line of
    [pidStr, cIdStr, aceitaStr, origem, destino, avaliacaoMotoristaStr, passageiroId] ->
        PassageiroViagem {
            pid = read pidStr,
            cId = read cIdStr,
            aceita = read aceitaStr,
            origemPass = origem,
            destino = destino,
            avaliacaoMtrst = read avaliacaoMotoristaStr,
            passageiroId = passageiroId
        }
    _ -> error "Invalid line format for PassageiroViagem"

instance ToRecord PassageiroViagem where
    toRecord entry = record
        [ toField (pid entry)
        , toField (cId entry)
        , toField (aceita entry)
        , toField (origemPass entry)
        , toField (destino entry)
        , toField (avaliacaoMtrst entry)
        , toField (passageiroId entry)
        ]
