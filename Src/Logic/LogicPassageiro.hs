module Src.Logic.LogicPassageiro where
    
    import Src.Schemas.Passageiro (Passageiro , getPassageiroByCpf, getPassageiroByEmail, cadastraPassageiro, confereSenha, removePassageiroByCpf, editPassageiroCSV)
    import Control.Monad (when, Monad (return))
    import Src.Util.Util(validarCPF, nullOrEmpty, validarEmail, validarGenero)

    cadastraPassageiroLogic :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Passageiro)
    cadastraPassageiroLogic nome cpf genero email telefone cep senha
        | nullOrEmpty nome = do
            putStrLn "Nome não pode ser vazio"
            return Nothing
        | validarCPF cpf = do
            putStrLn "CPF não atende aos requisitos"
            return Nothing
        | validarGenero genero = do
            putStrLn "Gênero deve ser masculino ou feminino"
            return Nothing
        | validarEmail email = do
            putStrLn "E-mail não atende aos requisitos"
            return Nothing
        | nullOrEmpty telefone = do
            putStrLn "Telefone não pode ser vazio"
            return Nothing
        | nullOrEmpty cep = do
            putStrLn "CEP não pode ser vazio"
            return Nothing
        | nullOrEmpty senha = do
            putStrLn "Senha não pode ser vazio"
            return Nothing
        | otherwise = cadastraPassageiro nome cpf genero email telefone cep senha

    editPassageiroCSVLogic :: String -> String -> String -> String -> IO (Maybe Passageiro)
    editPassageiroCSVLogic cpf senhaPassada coluna novoValor
        | validarCPF cpf = do
            putStrLn "CPF não atende aos requisitos"
            return Nothing
        | coluna `notElem` ["Telefone", "Cep", "Senha"] = do
            putStrLn "Escolha uma coluna válida para editar: Telefone, Cep ou Senha"
            return Nothing
        | nullOrEmpty novoValor = do
            putStrLn "O novo valor não pode ser vazio"
            return Nothing
        | otherwise = do
            resultado <- getPassageiroByCpf cpf
            case resultado of
                Just passageiro -> do
                    if confereSenha passageiro senhaPassada
                        then do
                            editPassageiroCSV cpf coluna novoValor
                            return (Just passageiro)
                        else do
                            putStrLn "Senha incorreta!"
                            return Nothing
                Nothing -> do
                    putStrLn "Passageiro não encontrado!"
                    return Nothing

    removePassageiroByCpfLogic :: String -> String -> IO (Maybe Passageiro)
    removePassageiroByCpfLogic cpf senhaPassada
        | validarCPF cpf = do
            putStrLn "CPF não atende aos requisitos"
            return Nothing
        | otherwise = do
            resultado <- getPassageiroByCpf cpf
            case resultado of
                Just passageiro -> do
                    if confereSenha passageiro senhaPassada
                        then do
                            removePassageiroByCpf cpf
                            return (Just passageiro)
                        else do
                            putStrLn "Senha incorreta!"
                            return Nothing
                Nothing -> do
                    putStrLn "Passageiro não encontrado!"
                    return Nothing

    getPassageiroByCpfLogic :: String -> String -> IO (Maybe Passageiro)
    getPassageiroByCpfLogic cpf senhaPassada
        | validarCPF cpf = do
            putStrLn "CPF não atende aos requisitos"
            return Nothing
        | otherwise = do
            resultado <- getPassageiroByCpf cpf
            case resultado of
                Just passageiro -> do
                    if confereSenha passageiro senhaPassada
                        then do
                            return (Just passageiro)
                        else do
                            putStrLn "Senha incorreta!"
                            return Nothing
                Nothing -> do
                    putStrLn "Passageiro não encontrado!"
                    return Nothing
    
    realizarLoginPassageiroLogic :: String -> String -> IO (Maybe Passageiro)
    realizarLoginPassageiroLogic email senha = do
        if validarEmail email then do
            putStrLn "Email não atende aos requisitos"
            return Nothing
        else do
            resultado <- getPassageiroByEmail email
            case resultado of
                Just passageiro -> do
                    if confereSenha passageiro senha
                        then do
                            return resultado
                        else do
                            putStrLn "Senha incorreta!"
                            return Nothing
                Nothing -> do
                    putStrLn "Email não cadastrado!"
                    return Nothing
                    