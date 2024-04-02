FROM haskell:9.6.4-buster

WORKDIR /app

# COPY ./batvolta-hs.cabal Main.hs ./

RUN cabal update

COPY . .

RUN cabal install --dependencies-only

RUN cabal build

ENTRYPOINT [ "cabal", "run" ]
