import Options.Applicative
import Data.Semigroup ((<>))

data DashboardCommand = 
    Historico
  | Ranking { regiao :: String }
  | Avaliar { usuario :: String, avaliacao :: Int, comentario :: Maybe String }
  | Estatisticas
  | Solicitacoes
  | Ofertas
  deriving Show

historico :: Parser DashboardCommand
historico = pure Historico

ranking :: Parser DashboardCommand
ranking = Ranking <$> strOption
  ( long "regiao"
  <> short 'r'
  <> metavar "REGIAO"
  <> help "Mostrar ranking de motoristas por região" )

avaliar :: Parser DashboardCommand
avaliar = Avaliar <$> strOption
  ( long "usuario"
  <> short 'u'
  <> metavar "USUARIO"
  <> help "Nome do usuário a ser avaliado" )
  <*> option auto
  ( long "avaliacao"
  <> short 'a'
  <> metavar "AVALIACAO"
  <> help "Avaliação (1 a 5)" )
  <*> optional (strOption
  ( long "comentario"
  <> short 'c'
  <> metavar "COMENTARIO"
  <> help "Comentário opcional" ))

estatisticas :: Parser DashboardCommand
estatisticas = pure Estatisticas

solicitacoes :: Parser DashboardCommand
solicitacoes = pure Solicitacoes

ofertas :: Parser DashboardCommand
ofertas = pure Ofertas

dashboard :: Parser DashboardCommand
dashboard = subparser $
    command "historico" (info historico (progDesc "Visualizar histórico de caronas")) <>
    command "ranking" (info ranking (progDesc "Ver ranking de motoristas por região")) <>
    command "avaliar" (info avaliar (progDesc "Avaliar usuário")) <>
    command "estatisticas" (info estatisticas (progDesc "Visualizar estatísticas pessoais")) <>
    command "solicitacoes" (info solicitacoes (progDesc "Visualizar solicitações de caronas pendentes")) <>
    command "ofertas" (info ofertas (progDesc "Visualizar ofertas de caronas ativas"))

main :: IO ()
main = do
  command <- execParser opts
  print command
  where
    opts = info (dashboard <**> helper)
      ( fullDesc
      <> progDesc "Interface de Linha de Comando para o Batvolta"
      <> header "batvolta - CLI para o aplicativo Batvolta" )
