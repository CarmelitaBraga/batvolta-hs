module Main where

import Data.Csv
import Data.Time
import Data.Time.Calendar 
import Src.Schema.CaronaSchema as CARONA
import Src.Logic.Carona as C
import Data.Time.LocalTime
-- import Src.Model.Carona
-- import Src.Controller.ControllerCarona
import Src.Util.Utils(validarData)
import GHC.IO (unsafePerformIO)

-- getBrazilCurrentDate :: IO Day
-- getBrazilCurrentDate = do
--     brazilTZ <- loadSystemTZ "America/Sao_Paulo"  -- Fuso horário do Brasil
--     brazilTime <- utcToLocalTimeTZ brazilTZ <$> getCurrentTime
--     return $ localDay brazilTime

main :: IO ()
main = do
--     putStrLn "Digite uma data no formato dd/mm/yyyy:"
--     input <- getLine
--     if validarData input
--         then putStrLn "A data inserida é válida."
--         else putStrLn "A data inserida não é válida."

    -- let caronaIds = [2]
    -- caronas <- CARONA.getCaronaById caronaIds
    -- mapM_ print caronas  -- Print the retrieved Carona objects
    -- CARONA.deleteCaronaById 1
    -- CARONA.criarCarona  getBrazilCurrentDate "Luana" "Everton" "André" [""] 19.99 5 [-1]
    -- caronas <- getCaronaById [0]
    -- csp <- CARONA.addPassageiro (head caronas) "Carmelita"
    -- print csp
    -- caronas <- getCaronaById [0]
    -- csp <- CARONA.addPassageiro (head caronas) "Caique"
    -- print csp

    -- caronas <- getCaronaById [0]
    -- csp <- CARONA.removerPassageiro (head caronas) "Caique"
    -- print csp

    gerarCarona "09:00" "02/03/2025" "Campina Grande" ["Joao Pessoa"] "091802112" 123.55 3

    -- a <- adicionarPassageiro 99 "Caique"
    -- print a

    -- a <- removerPassageiro 0 "Caique"
    -- putStrLn a

    CARONA.criarViagemPassageiro 0 False "Campina" "Gramado" 5 "55533399974"


    vs <- CARONA.getAllViagens
    print vs

    v <- CARONA.getViagemById [1]
    print v

    CARONA.deleteViagemById 15

    v0 <- CARONA.getViagemByColumn "pid" "0"
    print v0


    -- a <- removerPassageiro 99 "Caique"
    -- putStrLn a
    
    -- a <- removerPassageiro 0 "Lu"
    -- putStrLn a

    -- cs <- CARONA.getAllCaronas
    -- print cs

    -- aaaa <- C.infoCarona 0
    -- print aaaa

    -- dest <- infoCaronaByDestino "Home"
    -- print dest

    -- caronas <- mostrarCaronasPorDestino "Home"
    -- putStrLn caronas

    -- caronaPorId <- mostrarCaronaPorId "3"
    -- putStrLn caronaPorId

    -- caronasPassageiro <- mostrarCaronasPassageiro "Emma"
    -- putStrLn caronasPassageiro

    -- caronasMotorista <- mostrarCaronasMotorista "André"
    -- putStrLn caronasMotorista

    -- deletada <- deletarCaronaMotorista "0"
    -- putStrLn deletada