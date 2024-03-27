module Main where

import Data.Csv
import Data.Time
import Data.Time.Calendar 
import Src.Schema.CaronaSchema as CARONA
import Src.Logic.Carona as C
import Data.Time.LocalTime
import Src.Model.Carona
import Src.Controller.ControllerCarona

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

    -- let caronaIds = [2]
    -- caronas <- CARONA.getCaronaById caronaIds
    -- mapM_ print caronas  -- Print the retrieved Carona objects
    -- CARONA.deleteCaronaById 1
    -- CARONA.criarCarona timeOfDay today "Casa de Luana" "Casa de Everton" "André" 19.99
    
    -- cs <- CARONA.getAllCaronas
    -- print cs

    -- aaaa <- C.infoCarona 0
    -- print aaaa

    dest <- infoCaronaByDestino "Home"
    print dest

    caronas <- mostrarCaronasPorDestino "Home"
    putStrLn caronas

    caronaPorId <- mostrarCaronaPorId "3"
    putStrLn caronaPorId

    caronasPassageiro <- mostrarCaronasPassageiro "Emma"
    putStrLn caronasPassageiro

    caronasMotorista <- mostrarCaronasMotorista "André"
    putStrLn caronasMotorista

    deletada <- deletarCaronaMotorista "0"
    putStrLn deletada
