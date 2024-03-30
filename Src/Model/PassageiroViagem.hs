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
    pvId :: Int,
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
viagemToStr (PassageiroViagem p c a cam am pvId) =
  show p ++ "," ++
  show c ++ "," ++
  show a ++ "," ++
  intercalate ";" cam ++ "," ++
  show am ++ "," ++
  pvId

strToViagem :: String -> PassageiroViagem
strToViagem str =
  let parts = splitOn "," (filter (\c -> c /= '\r' && c /= '\\') str)
  in case parts of
        [p, c, a, cam, am, pvId] ->
          PassageiroViagem
            { pvId = read p,
              cId = read c,
              aceita = read a,
              caminho = splitOn ";" cam,
              avaliacaoMtrst = read am,
              passageiroId = pvId
            }
        _ -> error "Invalid input format for PassageiroViagem"

parseViagem :: String -> PassageiroViagem
parseViagem line = case splitOn "," line of
    [pvIdStr, cIdStr, aceitaStr, caminhoStr, avaliacaoMotoristaStr, passageiroId] ->
        PassageiroViagem {
            pvId = read pvIdStr,
            cId = read cIdStr,
            aceita = read aceitaStr,
            caminho = splitOn ";" caminhoStr,
            avaliacaoMtrst = read avaliacaoMotoristaStr,
            passageiroId = passageiroId
        }
    _ -> error "Invalid line format for PassageiroViagem"

instance ToRecord PassageiroViagem where
    toRecord entry = record
        [ toField (pvId entry)
        , toField (cId entry)
        , toField (aceita entry)
        , toField ("" :: String)
        , toField (avaliacaoMtrst entry)
        , toField (passageiroId entry)
        ]
