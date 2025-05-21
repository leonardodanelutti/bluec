FROM haskell:slim-bullseye

# Install bnfc.
RUN cabal update
ENV BNFC_VERSION="2.9.5"
RUN cabal install BNFC-$BNFC_VERSION

## Install yacc and bison.
RUN apt-get update
ENV FLEX_VERSION="2.6.4-8"
RUN apt-get install flex=$FLEX_VERSION -y
ENV BISON_VERSION="2:3.7.5+dfsg-1"
RUN apt-get install bison=$BISON_VERSION -y

# Install ssh
RUN apt-get install openssh-client -y