module Src.Model.PassageiroViagem where

import Data.List.Split (splitOn)
import Data.Csv (ToRecord, ToField, toRecord, toField, record)
import Data.List (intercalate)
import Data.ByteString.Char8 (pack)
import Debug.Trace

instance ToField Bool where
    toField True = pack "True"
    toField False = pack "False"

data PassageiroViagem = PassageiroViagem {
    pid :: Int,
    cId :: Int,
    aceita :: Bool,
    caminho :: [String],
    avaliacaoMtrst :: Int,
    passageiroId :: String
} deriving (Show, Eq)

instance Read PassageiroViagem where
  readsPrec _ str = [(strToViagem str, "")]

-- Function to convert a PassageiroViagem to a string
viagemToStr :: PassageiroViagem -> String
viagemToStr (PassageiroViagem p c a cam am pid) =
  show p ++ "," ++
  show c ++ "," ++
  show a ++ "," ++
  intercalate ";" cam ++ "," ++
  show am ++ "," ++
  pid

strToViagem :: String -> PassageiroViagem
strToViagem str =
  let parts = splitOn "," (filter (\c -> c /= '\r' && c /= '\\') str)
  in case parts of
        [p, c, a, cam, am, pid] ->
          PassageiroViagem
            { pid = read p,
              cId = read c,
              aceita = read a,
              caminho = splitOn ";" cam,
              avaliacaoMtrst = read am,
              passageiroId = pid
            }
        _ -> error "Invalid input format for PassageiroViagem"

parseViagem :: String -> PassageiroViagem
parseViagem line = case splitOn "," line of
    [pidStr, cIdStr, aceitaStr, caminhoStr, avaliacaoMotoristaStr, passageiroId] ->
        PassageiroViagem {
            pid = read pidStr,
            cId = read cIdStr,
            aceita = read aceitaStr,
            caminho = splitOn ";" caminhoStr,
            avaliacaoMtrst = read avaliacaoMotoristaStr,
            passageiroId = passageiroId
        }
    _ -> error "Invalid line format for PassageiroViagem"

instance ToRecord PassageiroViagem where
    toRecord entry = record
        [ toField (pid entry)
        , toField (cId entry)
        , toField (aceita entry)
        , toField ("" :: String)
        , toField (avaliacaoMtrst entry)
        , toField (passageiroId entry)
        ]
