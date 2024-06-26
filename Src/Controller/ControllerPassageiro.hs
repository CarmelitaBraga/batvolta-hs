module Src.Controller.ControllerPassageiro where

    import Src.Schemas.Passageiro(Passageiro (Passageiro))
    import Src.Logic.LogicPassageiro(cadastraPassageiroLogic, removePassageiroByCpfLogic, editPassageiroCSVLogic, getPassageiroByCpfLogic, realizarLoginPassageiroLogic)
    import Src.Schemas.Notificacao (getBy, passageiroGetBy)
    import Src.Model.NotificacaoModel (Notificacao)

    realizarCadastroPassageiro :: String -> String -> String -> String -> String -> String -> String -> IO (Maybe Passageiro)
    realizarCadastroPassageiro = cadastraPassageiroLogic

    cancelarCadastroPassageiro :: String -> String -> IO (Maybe Passageiro)
    cancelarCadastroPassageiro = removePassageiroByCpfLogic

    atualizarCadastroPassageiro :: String -> String -> String -> String -> IO (Maybe Passageiro)
    atualizarCadastroPassageiro = editPassageiroCSVLogic

    visualizarInfoCadastroPassageiro :: String -> String -> IO (Maybe Passageiro)
    visualizarInfoCadastroPassageiro = getPassageiroByCpfLogic

    realizarLoginPassageiro :: String -> String -> IO (Maybe Passageiro)
    realizarLoginPassageiro = realizarLoginPassageiroLogic

    carregaNotificacoes :: String -> IO [Notificacao]
    carregaNotificacoes = passageiroGetBy