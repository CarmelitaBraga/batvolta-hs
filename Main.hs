module Main where

import Data.Csv
import Data.Time
import Data.Time.Calendar 
import Src.Schema.CaronaSchema as CARONA
import Src.Logic.CaronaLogic as C
import Src.Schema.PassageiroViagemSchema as V
import Data.Time.LocalTime
-- import Src.Model.Carona
import Src.Controller.ControllerCarona
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

    C.gerarCarona "19:01" "10/10/2024" "Amazonia" ["Joao Pessoa", "Ceara", "Rio de Janeiro", "Curitiba"] "091802112" 200.90 1

 
    -- Joao Pessoa -> Stadunido no
    -- Stadunido -> Curitiba no
    -- Gramado -> Ceara yes
    -- Ceara -> Minas Gerais 
    -- d0 <- C.filtrarCaronaOriDest "Ceara" "Curitiba"
    -- print d0

    -- d1 <- C.filtrarCaronaOriDest "Joao Pessoa" "Curitiba"
    -- print d1

    -- d2 <- C.filtrarCaronaOriDest "Joao Pessoa" "Ceara"
    -- print d2

    -- mostrarCaronasOrigemDestino "Joao Pessoa" "Ceara"

    -- d3 <- mostrarCaronasOrigemDestino "Curitiba" "Para"
    -- print d3

    -- mostrarCaronasOrigemDestino "Curitiba" "Para"

    -- mostrarCaronasOrigemDestino "Amazonia" "Stadunido"

    -- d5 <- C.filtrarCaronaOriDest "Stadunido" "Curitiba"
    -- print d5

    -- d6 <- C.filtrarCaronaOriDest "Gramado" "Ceara"
    -- print d6

    mostrarCaronasOrigemDestino "Joao Pessoa" "Patos"

    finalizarCaronaStatus 6
    inicializarCaronaStatus 77
    deletarCaronaPorId 47

    -- d7 <- C.filtrarCaronaOriDest "Ceara" "Minas Gerais"
    -- print d7

    -- d8 <- C.filtrarCaronaOriDest "Joao Pessoa" "Patos"
    -- print d8 --warning

    -- a <- adicionarPassageiro 99 "Caique"
    -- print a

    -- a <- removerPassageiro 0 "Caique"
    -- putStrLn a


    -- V.criarViagemPassageiro 0 False "Campina" "Gramado" 5 "55533399974"


    -- vs <- V.getAllViagens
    -- print vs

    -- v <- V.getViagemById [1]
    -- print v

    -- V.deleteViagemById 15

    -- v0 <- V.getViagemByColumn "pid" "0"
    -- print v0

    -- up <- V.updateSolicitacaoViagem 4 "true"
    -- print up

    -- avalia <- V.updateAvaliacaoViagem 7 1
    -- print avalia

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