module Src.CLI.MotoristaCLI where

--Imports
--Controllers
import Src.Controller.ControllerMotorista(realizarCadastroMotorista, cancelarCadastroMotorista, atualizarCadastroMotorista, visualizarInfoCadastroMotorista, realizarLoginMotorista,carregaNotificacoes)
import Src.Controller.ControllerCarona as CONTROLLER
--Models
import Src.Model.MotoristaModel(Motorista (..), getCpf)
--Bibliotecas
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import qualified Src.Logic.CaronaLogic as CONTROLLER
import Src.Schemas.Notificacao(insereNotificacaoPassageiro)
import Src.Model.Carona (Carona(cid))
import Data.Char (toLower)
import Src.Util.Utils
import Data.IORef 

-- Motorista Logado
type MotoristaRef = IORef (Maybe Motorista)
            
-- Implementação dos menus
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\nSelecione uma opção:"
    putStrLn "1 - Cadastro de Motorista"
    putStrLn "2 - Login"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> void menuCadastrarMotorista
        "2" -> do
            motoristaRef <- newIORef Nothing
            void $ menuRealizarLogin motoristaRef
        "0" -> do
            putStrLn "Saindo..."
        _   -> do
            putStrLn "Opção inválida!"
            menuPrincipal

menuOpcoesMotorista :: MotoristaRef -> IO ()
menuOpcoesMotorista motoristaRef = do
    putStrLn "\nOpções do Motorista:"
    putStrLn "1 - Atualizar Cadastro"
    putStrLn "2 - Cancelar Cadastro"
    putStrLn "3 - Visualizar Informações"
    putStrLn "4 - Carregar historico de Notificações"
    putStrLn "5 - Menu de Caronas"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> void (menuAtualizarCadastro motoristaRef)
        "2" -> void (menuCancelarCadastro motoristaRef)
        "3" -> void (menuVisualizarInfo motoristaRef)
        "4" -> void (menuCarregarNotificacoes motoristaRef)
        "5" -> void (menuPrincipalCaronaMotorista motoristaRef)
        "0" -> menuPrincipal
        _   -> do
            putStrLn "Opção inválida!"
            menuOpcoesMotorista motoristaRef


menuCadastrarMotorista :: IO ()
menuCadastrarMotorista = do
    putStrLn "\nCadastrar Motorista"
    cpf <- inputString "Digite o CPF do motorista: "
    cep <- inputString "Digite o CEP: "
    nome <- inputString "Digite o nome: "
    email <- inputString "Digite o e-mail: "
    telefone <- inputString "Digite o telefone: "
    senha <- inputString "Digite a senha: "
    cnh <- inputString "Digite a CNH: "
    regiao <- inputString "Digite a regiao: "
    genero <- inputString "Digite o gênero(F/M/NB): "
    resultado <- realizarCadastroMotorista cpf cep nome email telefone senha cnh genero regiao
    case resultado of
        Just motorista -> putStrLn "Motorista cadastrado com sucesso!"
        Nothing -> putStrLn "Erro ao cadastrar motorista."
    menuPrincipal

menuCancelarCadastro :: MotoristaRef -> IO ()
menuCancelarCadastro motoristaRef = do
    putStrLn "\nCancelar Cadastro de Motorista"
    motoristaMaybe <- readIORef motoristaRef
    let cpf = getCpf motoristaMaybe
    senha <- inputString "Digite sua senha:"
    resultado <- cancelarCadastroMotorista cpf senha
    case resultado of
        Just motorista -> do 
            putStrLn "Cadastro de motorista cancelado com sucesso!"
            menuPrincipal        
        Nothing -> do
            putStrLn "Erro ao cancelar cadastro de motorista."
            menuOpcoesMotorista motoristaRef
    

menuAtualizarCadastro ::MotoristaRef -> IO ()
menuAtualizarCadastro motoristaRef = do
    putStrLn "\nAtualizar Cadastro de Motorista"
    motoristaMaybe <- readIORef motoristaRef
    let cpf = getCpf motoristaMaybe
    senha <- inputString "Digite sua senha:"
    putStrLn "Selecione o atributo a ser atualizado:"
    putStrLn "1 - Telefone"
    putStrLn "2 - Cep"
    putStrLn "3 - Senha"
    opcao <- inputString "Opção: "
    novoValor <- inputString "Digite o novo valor: "
    resultado <- case opcao of
        "1" -> atualizarCadastroMotorista cpf senha "Telefone" novoValor
        "2" -> atualizarCadastroMotorista cpf senha "Cep" novoValor
        "3" -> atualizarCadastroMotorista cpf senha "Senha" novoValor
        _   -> return Nothing
    case resultado of
        Just motorista -> putStrLn "Cadastro de motorista atualizado com sucesso!"
        Nothing -> putStrLn "Erro ao atualizar cadastro de motorista."
    menuOpcoesMotorista motoristaRef

menuVisualizarInfo :: MotoristaRef -> IO ()
menuVisualizarInfo motoristaRef = do
    putStrLn "\nInformações do motorista:"
    motoristaMaybe <- readIORef motoristaRef
    print motoristaMaybe
    menuOpcoesMotorista motoristaRef


menuRealizarLogin :: MotoristaRef -> IO ()
menuRealizarLogin motoristaRef = do
    putStrLn "\nRealizar Login de Motorista"
    email <- inputString "Digite o e-mail: "
    senha <- inputString "Digite a senha: "
    resultado <- realizarLoginMotorista email senha
    case resultado of
        Just motorista -> do
            putStrLn "Login bem-sucedido!"
            writeIORef motoristaRef resultado
            menuOpcoesMotorista motoristaRef
        Nothing -> do 
            menuPrincipal


menuCarregarNotificacoes :: MotoristaRef -> IO ()
menuCarregarNotificacoes motoristaRef = do
    putStrLn "\nCarregar Notificações do Motorista"
    motoristaMaybe <- readIORef motoristaRef
    let cpfMotorista = getCpf motoristaMaybe
    notificacoes <- carregaNotificacoes cpfMotorista
    putStrLn "Notificações:"
    mapM_ print notificacoes
    menuOpcoesMotorista motoristaRef


-- Menu Para Caronas
menuPrincipalCaronaMotorista :: MotoristaRef -> IO ()
menuPrincipalCaronaMotorista motoristaRef = do
    putStrLn "\nSelecione uma opção:"
    putStrLn "1 - Criar uma Carona"
    putStrLn "2 - Iniciar Carona"
    putStrLn "3 - Finalizar Carona"
    putStrLn "4 - Aceitar/Recusar passageiro"
    putStrLn "5 - Cancelar uma Carona"
    putStrLn "6 - Visualizar caronas"
    putStrLn "7 - Avaliar carona finalizada"
    putStrLn "0 - Sair"
    opcao <- getLine
    case opcao of
        "1" -> menuCriarCarona motoristaRef
        "2" -> menuIniciarCarona motoristaRef
        "3" -> menuFinalizarCarona motoristaRef
        "4" -> menuAceitarRecusarPassageiro motoristaRef
        "5" -> menuCancelarCarona motoristaRef
        "6" -> menuVisualizarCarona motoristaRef
        "7" -> menuAvaliarCarona motoristaRef
        "0" -> do
            putStrLn "Saindo..."
            menuOpcoesMotorista motoristaRef
        _   -> do
            putStrLn "Opção inválida!"
            menuPrincipalCaronaMotorista motoristaRef
    

menuCriarCarona :: MotoristaRef -> IO ()
menuCriarCarona motoristaRef = do
    putStrLn "\nCriar uma Carona"
    hora <- inputString "Digite a hora (no formato HH:MM): "
    date <- inputString "Digite a data (no formato DD/MM/AAAA): "
    origem <- inputString "Digite a origem da viagem: "
    destinos <- pedirDestinos
    valor <- inputDouble "Digite o valor (use '.' para casa decimal): "
    numPassageirosMaximos <- inputInt "Digite a quantidade máximas de passageiros: "
    motoristaMaybe <- readIORef motoristaRef
    let motorista = getCpf motoristaMaybe
    CONTROLLER.criarCaronaMotorista hora date (origem:destinos) motorista valor numPassageirosMaximos
    menuPrincipalCaronaMotorista motoristaRef

menuCancelarCarona :: MotoristaRef -> IO()
menuCancelarCarona motoristaRef = do
    motoristaMaybe <- readIORef motoristaRef
    let motorista = getCpf motoristaMaybe
    possuiCarona <- CONTROLLER.possuiCaronaNaoIniciadaController motorista

    if possuiCarona then do
        putStrLn "Qual carona você deseja cancelar:"
        caronas <- CONTROLLER.infoCaronasNaoIniciadas motorista
        putStrLn caronas

        cId <- inputInt "Digite o Id da carona: "
        caronaPertenceMotorista <- CONTROLLER.checarCaronaDeMotorista cId motorista
        if caronaPertenceMotorista
            then do
                deletar <- CONTROLLER.deletarCaronaPorId motorista cId
                putStrLn deletar
            else do
                putStrLn "Carona não pertence a motorista!"
    else do
        putStrLn "Não existem caronas possíveis de se cancelar!"
    menuPrincipalCaronaMotorista motoristaRef

pedirDestinos :: IO [String]
pedirDestinos = menuPedirDestinos []

menuPedirDestinos :: [String] -> IO [String]
menuPedirDestinos destinos = do
    maybeDestino <- inputString $ "Digite a " ++ show (length destinos + 1) ++ "ª cidade (aperte apenas enter para terminar de inserir destinos)(Digite sem caracteres especiais, exemplo: ´,~,...): "
    if null maybeDestino
        then return destinos
        else menuPedirDestinos (destinos ++ [maybeDestino])

menuIniciarCarona :: MotoristaRef -> IO ()
menuIniciarCarona motoristaRef = do
    motoristaMaybe <- readIORef motoristaRef
    let motorista = getCpf motoristaMaybe
    possuiCarona <- CONTROLLER.possuiCaronaNaoIniciadaController motorista

    if possuiCarona then do
        putStrLn "Qual carona você deseja iniciar:"
        caronas <- CONTROLLER.infoCaronasNaoIniciadas motorista
        putStrLn caronas

        cId <- inputInt "Digite o Id da carona: "
        caronaPertenceMotorista <- CONTROLLER.checarCaronaDeMotorista cId motorista
        if caronaPertenceMotorista
            then do
                deletar <- CONTROLLER.inicializarCaronaStatus cId
                putStrLn deletar
            else do
                putStrLn "Carona não pertence a motorista!"

    else do
        putStrLn "Não existem caronas possíveis de se iniciar!"
    menuPrincipalCaronaMotorista motoristaRef

menuFinalizarCarona :: MotoristaRef -> IO()
menuFinalizarCarona motoristaRef = do
    motoristaMaybe <- readIORef motoristaRef
    let motorista = getCpf motoristaMaybe
    possuiCarona <- CONTROLLER.possuiCaronaEmAndamentoController motorista
    if possuiCarona then do
        putStrLn "Qual carona você deseja Finalizar:"
        caronas <- CONTROLLER.infoCaronasEmAndamento motorista
        putStrLn caronas

        cId <- inputInt "Digite o Id da carona: "
        caronaPertenceMotorista <- CONTROLLER.checarCaronaDeMotorista cId motorista
        if caronaPertenceMotorista
            then do
                deletar <- CONTROLLER.finalizarCaronaStatus cId
                putStrLn deletar
            else do
                putStrLn "Carona não pertence a motorista!"
    else do
        putStrLn "Não existem caronas possíveis de se finalizar!"
    menuPrincipalCaronaMotorista motoristaRef

menuAceitarRecusarPassageiro :: MotoristaRef -> IO ()
menuAceitarRecusarPassageiro motoristaRef = do
    motoristaMaybe <- readIORef motoristaRef
    let motorista = getCpf motoristaMaybe
    possuiCarona <- possuiCaronasPassageirosViagemFalse motorista
    if possuiCarona then do
        putStrLn "Qual carona você deseja olhar os passageiros:"
        caronas <- infoCaronaPassageirosViagemFalse motorista
        putStrLn caronas
        cId <- inputInt "Digite o Id: "
        temPassageiroViagemFalse <- possuiPassageiroViagemFalse cId
        if temPassageiroViagemFalse then do
            putStrLn "Qual passageiro você deseja aceitar/recusar?"
            passageirosFalse <- infoPassageiroViagemFalse cId
            putStrLn passageirosFalse
            pvId <- inputInt "Digite o Id: "
            temEssePassageiro <- possuiPassageiroViagem cId pvId
            if temEssePassageiro then do
                aceitarOuRecusar <- inputBoolean "Você deseja aceitar ou recusar: "
                response <- aceitarOuRecusarPassageiro motorista pvId aceitarOuRecusar
                insereNotificacaoPassageiro motorista (show pvId) cId (response ++ "na carona de Id:" ++ show cId)
                putStrLn response
                menuPrincipalCaronaMotorista motoristaRef
            else do
                putStrLn "Esse passageiro não está disponível!"
                menuPrincipalCaronaMotorista motoristaRef
        else do
            putStrLn "Essa carona não está disponível!"
            menuPrincipalCaronaMotorista motoristaRef
    else do
        putStrLn "Não existem caronas disponíveis para aceitar ou recusar passageiros!"
        menuPrincipalCaronaMotorista motoristaRef

menuVisualizarCarona :: MotoristaRef -> IO()
menuVisualizarCarona motoristaRef = do
    motoristaMaybe <- readIORef motoristaRef
    let motorista = getCpf motoristaMaybe
    possuiCaronas <- motoristaPossuiCaronas motorista
    if possuiCaronas then do
        putStrLn "Suas caronas disponiveis: "
        caronas <- CONTROLLER.mostrarCaronasMotorista motorista
        putStrLn caronas     
    else do
        putStrLn "Não existem caronas disponíveis para aceitar ou recusar passageiros!"

    menuPrincipalCaronaMotorista motoristaRef

menuAvaliarCarona :: MotoristaRef -> IO()
menuAvaliarCarona motoristaRef = do
    motoristaMaybe <- readIORef motoristaRef 
    let motoristaCpf = getCpf motoristaMaybe
    caronas <- caronasSemAvaliacao motoristaCpf
    if not (null caronas) then do
        putStrLn caronas
        caronaId <- inputInt "Digite o Id da carona: "
        avaliacao <- inputInt "Digite a avaliação"
        resultado <- avaliarCarona motoristaCpf caronaId avaliacao
        putStrLn resultado
    else do
        putStrLn "Não existem caronas disponíveis para avaliar!"
    
    menuPrincipalCaronaMotorista motoristaRef