# Batvolta

Bem-vindo ao Batvolta! Este é um projeto desenvolvido em Haskell que visa fornecer uma plataforma para caronas, permitindo que usuários ofereçam e solicitem caronas de forma eficiente. Este documento contém instruções sobre como utilizar e contribuições do projeto.

## Funcionalidades
O escopo deste projeto inclui as seguintes funcionalidades:

1. Cadastro de usuários: Permitir que os usuários se cadastrem na plataforma fornecendo informações básicas, como nome, e-mail e senha.
2. Login de usuários: Usuários (motoristas e caronas) podem fazer login para acessar as funcionalidades da aplicação.
3. Oferta de caronas: Permitir que os usuários ofereçam caronas especificando origem, destino, horário de partida, número de assentos disponíveis e preferências adicionais, como gênero dos passageiros aceitos.
4. Solicitação de caronas: Permitir que os usuários solicitem caronas informando origem, destino e horário desejado.
5. Dashboard por usuário:
    - Histórico: O dashboard irá exibir o histórico de caronas do usuário.
    - Ranking de motoristas por região: Permitir que os usuários vejam um ranking dos principais motoristas por região.
    - Rating (avaliação de usuários): Os usuários podem avaliar-se mutuamente, na relação motorista-carona.
6. Alocação de passageiros: Utilizar uma lógica de grafos para determinar a alocação eficiente de passageiros nas caronas disponíveis, levando em consideração fatores como disponibilidade e preferência de trajetos, horário e preferências dos usuários.
7. CRUD de caronas: Implementar operações de criação, leitura, atualização e exclusão de caronas para manter a integridade e atualização das informações na plataforma.

## Uso

### Instalação do Haskell e Cabal
1. Requerimentos: É necessário ter o `ghc` e o `cabal` instalados. Você pode baixar e instalar a versão mais recente do Haskell Platform no site oficial.

2. Verifique a instalação: Após a instalação, abra um terminal e verifique se o Haskell está instalado corretamente executando o seguinte comando:
```sh
ghc --version
cabal --version
```
Isso deve exibir a versão do compilador GHC instalada e do cabal, sistema de construção e empacotamento para Haskell.

3. Atualização: atualize o cabal com os pacotes mais atuais.
```sh
cabal update
```
Isso garantirá que você tenha a versão mais recente do Cabal instalada em seu sistema.

### Executando o Projeto
Depois de instalar o Haskell e o Cabal, você pode executar o projeto. Navegue até o diretório do projeto e execute os seguintes comandos:
```sh
cabal build
cabal run
```
Isso irá compilar o projeto e executá-lo. Siga as instruções no terminal para interagir com o aplicativo Batvolta.

Para começar, execute o arquivo principal do programa:
```sh
cabal run
```

Isso iniciará o aplicativo e você será recebido com o menu principal.

```Bem-vindo ao Batvolta!
Escolha o tipo de usuário:
1 - Motorista
2 - Passageiro
3 - Dashboard
0 - Sair
```

Digite o número correspondente à opção desejada e pressione Enter para prosseguir.

## Contribuição

Este projeto foi desenvolvido por:
- [André Almeida](https://github.com/AndreFelipeAlmeida)
- [Caique Campelo](https://github.com/Cazans)
- [Carmelita Braga](https://github.com/CarmelitaBraga)
- [Filipe Ferreira](https://github.com/filipe1417)
- [Gabriel Guimarães](https://github.com/Gaabrielg1)
- [Ian Evangelista](https://github.com/ianzx15)
