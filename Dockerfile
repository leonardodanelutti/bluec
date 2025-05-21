FROM haskell:slim-bullseye

# Install bnfc.
RUN cabal update && cabal install BNFC

## Install yacc and bison.
RUN apt-get update
RUN apt-get install flex -y
RUN apt-get install bison -y

# Install ssh
RUN apt-get install openssh-client -y