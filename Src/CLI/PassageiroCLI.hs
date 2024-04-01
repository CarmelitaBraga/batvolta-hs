module Src.CLI.PassageiroCLI where

    import System.IO
    import Src.Schemas.Passageiro(Passageiro, getPassageiroByCpf, getCLICpf)
    import Src.Controller.ControllerPassageiro(realizarCadastroPassageiro, cancelarCadastroPassageiro, atualizarCadastroPassageiro,visualizarInfoCadastroPassageiro,realizarLoginPassageiro, carregaNotificacoes)
    import Data.IORef
    import Control.Monad
    import Src.Controller.ControllerCarona as CONTROLLER
    import Src.Util.Utils
    -- Passageiro
    type PassageiroRef = IORef (Maybe Passageiro)
    
    menuPrincipalPassageiro :: IO ()
    menuPrincipalPassageiro = do
        putStrLn "\nSelecione uma opção: "
        putStrLn "1 - Cadastro de Passageiro"
        putStrLn "2 - Login"
        putStrLn "0 - Sair"
        opcao <- getLine
        case opcao of
            "1" -> void menuCadastrarPassageiro
            "2" -> do
                passageiroRef <- newIORef Nothing
                void $ menuRealizarLoginPassageiro passageiroRef
            "0" -> putStrLn "Saindo..."
            _   -> do
                putStrLn "Opção inválida!"
                menuPrincipalPassageiro

    menuOpcoesPassageiro :: PassageiroRef -> IO ()
    menuOpcoesPassageiro passageiroRef = do
        passageiro <- readIORef passageiroRef
        putStrLn "\nOpções do Passageiro: "
        putStrLn "1 - Atualizar Cadastro"
        putStrLn "2 - Cancelar Cadastro"
        putStrLn "3 - Visualizar Informações"
        putStrLn "4 - Visualizar Notificações"
        putStrLn "5 - Menu Caronas"
        putStrLn "0 - Voltar Menu Principal"
        opcao <- getLine
        case opcao of
            "1" -> void $ menuAtualizarCadastroPassageiro passageiroRef
            "2" -> void $ menuCancelarCadastroPassageiro passageiroRef
            "3" -> void $ menuVisualizarInfoPassageiro passageiroRef
            "4" -> void $ menuVisualizarNotificacoes passageiroRef
            "5" -> void $ menuPrincipalPassageiroCarona passageiroRef
            "0" -> menuPrincipalPassageiro
            _   -> do
                putStrLn "Opção inválida!"
                menuOpcoesPassageiro passageiroRef

    menuCadastrarPassageiro :: IO ()
    menuCadastrarPassageiro = do
        putStrLn "\nCadastrar Passageiro"
        nome <- inputString "Digite o nome: "
        cpf <- inputString "Digite o CPF: "
        genero <- inputString "Digite o gênero (f ou m): "
        email <- inputString "Digite o e-mail: "
        telefone <- inputString "Digite o telefone: "
        cep <- inputString "Digite o CEP: "
        senha <- inputString "Digite a senha: "
        resultado <- realizarCadastroPassageiro nome cpf genero email telefone cep senha
        case resultado of
            Just passageiro -> putStrLn "Passageiro cadastrado com sucesso!"
            Nothing -> putStrLn "Erro ao cadastrar passageiro."
        menuPrincipalPassageiro

    menuCancelarCadastroPassageiro :: PassageiroRef -> IO ()
    menuCancelarCadastroPassageiro passageiroRef = do
        putStrLn "\nCancelar Cadastro de Passageiro"
        passageiroMaybe <- readIORef passageiroRef
        let cpf = getCLICpf passageiroMaybe
        senha <- inputString "Digite sua senha: "
        resultado <- cancelarCadastroPassageiro cpf senha
        case resultado of
            Just passageiro -> do
                putStrLn "Cadastro de passageiro cancelado com sucesso!"
                menuPrincipalPassageiro
            Nothing -> do
                putStrLn "Erro ao cancelar cadastro de passageiro."
                menuOpcoesPassageiro passageiroRef

    menuAtualizarCadastroPassageiro :: PassageiroRef -> IO ()
    menuAtualizarCadastroPassageiro passageiroRef = do
        putStrLn "\nAtualizar Cadastro de Passageiro"
        passageiroMaybe <- readIORef passageiroRef
        let cpf = getCLICpf passageiroMaybe
        senha <- inputString "Digite sua senha: "
        putStrLn "Selecione o atributo a ser atualizado: "
        putStrLn "1 - Telefone"
        putStrLn "2 - Cep"
        putStrLn "3 - Senha"
        opcao <- inputString "Opção: "
        novoValor <- inputString "Digite o novo valor: "
        resultado <- case opcao of
            "1" -> atualizarCadastroPassageiro cpf senha "Telefone" novoValor
            "2" -> atualizarCadastroPassageiro cpf senha "Cep" novoValor
            "3" -> atualizarCadastroPassageiro cpf senha "Senha" novoValor
            _   -> return Nothing
        case resultado of
            Just passageiro -> putStrLn "Cadastro de passageiro atualizado com sucesso!"
            Nothing -> putStrLn "Erro ao atualizar cadastro de passageiro."
        menuOpcoesPassageiro passageiroRef

    menuVisualizarInfoPassageiro :: PassageiroRef -> IO ()
    menuVisualizarInfoPassageiro passageiroRef = do
        passageiro <- readIORef passageiroRef
        putStrLn "Informações do passageiro: "
        print passageiro
        menuOpcoesPassageiro passageiroRef

    menuRealizarLoginPassageiro :: PassageiroRef -> IO ()
    menuRealizarLoginPassageiro passageiroRef = do
        putStrLn "\nRealizar Login de Passageiro"
        email <- inputString "Digite o e-mail: "
        senha <- inputString "Digite a senha: "
        resultado <- realizarLoginPassageiro email senha
        case resultado of
            Just passageiro -> do
                putStrLn "Login realizado com sucesso!"
                writeIORef passageiroRef resultado
                menuOpcoesPassageiro passageiroRef
            Nothing -> do
                putStrLn "Erro ao realizar login."
                menuPrincipalPassageiro

    menuVisualizarNotificacoes :: PassageiroRef -> IO ()
    menuVisualizarNotificacoes passageiroRef = do
        putStrLn "\n** Histórico de Notificações **\n"
        passageiroMaybe <- readIORef passageiroRef
        let cpfPassageiro = getCLICpf passageiroMaybe
        notificacoes <- carregaNotificacoes cpfPassageiro
        mapM_ print notificacoes
        menuOpcoesPassageiro passageiroRef

    --Menu Carona Passageiro

    menuPrincipalPassageiroCarona :: PassageiroRef -> IO ()
    menuPrincipalPassageiroCarona passageiroRef = do
        putStrLn "\nSelecione uma opção:"
        putStrLn "1 - Procurar Carona"
        putStrLn "2 - Cancelar Carona"
        putStrLn "3 - Ver minhas viagens"
        putStrLn "4 - Embarcar na Carona"
        putStrLn "5 - Desembarcar da Carona"
        putStrLn "6 - Avaliar Motorista"
        putStrLn "0 - Voltar"

        opcao <- getLine
        case opcao of
            "1" -> menuProcurarCarona passageiroRef
            "2" -> menuCancelarCarona passageiroRef
            "3" -> menuMostrarCaronas passageiroRef
            "4" -> menuEmbarcarCaronas passageiroRef
            "5" -> menuDesembarcarCaronas passageiroRef
            "6" -> menuAvaliarMotorista passageiroRef
            "0" -> menuOpcoesPassageiro passageiroRef
            _   -> do
                putStrLn "Opção inválida!"
                menuPrincipalPassageiroCarona passageiroRef

    menuProcurarCarona :: PassageiroRef -> IO ()
    menuProcurarCarona passageiroRef = do
        passageiroMaybe <- readIORef passageiroRef
        let passageiroCpf = getCLICpf passageiroMaybe
        origem <- inputString "De onde a carona deve partir?(Digite sem caracteres especiais, exemplo: ´,~,...) "
        destino <- inputString "Onde a carona deve chegar?(Digite sem caracteres especiais, exemplo: ´,~,...) "
        existeCaronas <- CONTROLLER.possuiCaronasOrigemDestinoController origem destino

        if existeCaronas then do
            caronas <- CONTROLLER.mostrarCaronasDisponiveisOrigemDestino origem destino
            putStrLn caronas

            cId <- inputInt "Qual carona deseja solicitar (Digite o Id da carona) (Digite -1 para não escolher nenhuma): "
            if cId == -1 then
                menuPrincipalPassageiroCarona passageiroRef
            else do
                maybeCaronaEscolhida <- CONTROLLER.solicitarCaronaPassageiro cId passageiroCpf origem destino
                putStrLn maybeCaronaEscolhida
                menuPrincipalPassageiroCarona passageiroRef

        else do
            putStrLn "Não existem caronas para essa origem e destino!"
            menuPrincipalPassageiroCarona passageiroRef

    menuCancelarCarona :: PassageiroRef -> IO ()
    menuCancelarCarona passageiroRef = do
        passageiroMaybe <- readIORef passageiroRef
        let passageiroCpf = getCLICpf passageiroMaybe
        caronas <- mostrarViagemPassageiro passageiroCpf
        putStrLn caronas
        cId <- inputInt "Qual carona deseja cancelar (Digite o Id da carona) (Digite -1 para não escolher nenhuma): "
        if cId == -1 then
            menuPrincipalPassageiroCarona passageiroRef
        else do
            cancelarCaronaPassageiro cId passageiroCpf
            menuPrincipalPassageiroCarona passageiroRef


    menuMostrarCaronas :: PassageiroRef -> IO ()
    menuMostrarCaronas passageiroRef = do
        passageiroMaybe <- readIORef passageiroRef
        let passageiroCpf = getCLICpf passageiroMaybe
        resultado <- mostrarViagemPassageiro passageiroCpf
        putStrLn resultado
        menuPrincipalPassageiroCarona passageiroRef

    menuDesembarcarCaronas :: PassageiroRef -> IO()
    menuDesembarcarCaronas passageiroRef = do
        passageiroMaybe <- readIORef passageiroRef
        let passageiroCpf = getCLICpf passageiroMaybe

        let passageiroCpf = getCLICpf passageiroMaybe
        resultado <- mostrarCaronasPassageiro passageiroCpf
        if resultado /= "\n" then do
            print resultado
            putStrLn "Digite o ID da carona:"
            idCaronaStr <- getLine
            let idCarona = read idCaronaStr :: Int
            desembarcarPassageiro idCarona passageiroCpf
            menuPrincipalPassageiroCarona passageiroRef
        else do
            putStrLn "Nenhuma carona disponivel para desembarcar"
            menuPrincipalPassageiroCarona passageiroRef

    menuEmbarcarCaronas :: PassageiroRef -> IO()
    menuEmbarcarCaronas passageiroRef = do
        passageiroMaybe <- readIORef passageiroRef
        let passageiroCpf = getCLICpf passageiroMaybe

        putStrLn "Escolha a carona para embarcar"
        resultado <- mostrarViagemPassageiro passageiroCpf
        putStrLn resultado

        putStrLn "Digite o ID da carona:"
        idCaronaStr <- getLine
        let idCarona = read idCaronaStr :: Int
        embarcarPassageiro idCarona passageiroCpf
        menuPrincipalPassageiroCarona passageiroRef
    
    menuAvaliarMotorista :: PassageiroRef -> IO()
    menuAvaliarMotorista passageiroRef = do
        passageiroMaybe <- readIORef passageiroRef
        let passageiroCpf = getCLICpf passageiroMaybe
        caronas <- getViagemSemAvaliacao passageiroCpf
        if caronas /= "" then do
                putStrLn caronas
                idCarona <- inputInt "Digite o ID da carona que deseja avaliar:"
                avaliacao <- inputInt "Digite a avaliação do motorista:"
                result <- avaliarMotorista idCarona passageiroCpf avaliacao
                putStrLn result
        else do
                putStrLn "Nenhuma carona encontrada para avaliação!"
        menuPrincipalPassageiroCarona passageiroRef

