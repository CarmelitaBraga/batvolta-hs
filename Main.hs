module Main where

import Data.Csv
import Data.Time
import Data.Time.Calendar 
import Src.Schema.CaronaSchema as CARONA
import Data.Time.LocalTime

main :: IO ()
main = do
    -- Obtém a data e hora atuais
    currentTime <- getCurrentTime

    -- Converte a UTCTime para o tipo Day (extrai apenas a data)
    let today = utctDay currentTime

        -- Obtém a hora atual no fuso horário local
    zonedTime <- getZonedTime

    -- Extrai o horário atual do ZonedTime
    let timeOfDay = localTimeOfDay (zonedTimeToLocalTime zonedTime)

    let caronaIds = [2]
    caronas <- CARONA.getCaronaById caronaIds
    CARONA.apagarCarona 1
    CARONA.criarCarona timeOfDay today "Casa de Luana" "Casa de Everton" "André" 19.99
