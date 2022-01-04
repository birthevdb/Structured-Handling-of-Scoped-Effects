# FROM haskell:8.10
# FROM ubuntu:latest
ARG OCAML_VERSION="4.12"
FROM ocaml/opam:debian-ocaml-${OCAML_VERSION}
ARG OCAML_VERSION

RUN apt update && apt install -y ghc

# Clone repo to get files
RUN git clone https://github.com/birthevdb/Structured-Handling-of-Scoped-Effects.git
WORKDIR Structured-Handling-of-Scoped-Effects
ENV PATH /root/.cabal/bin:/root/.local/bin:/opt/ghc/${GHC}/bin:$PATH

# Run the examples
CMD ["ocaml examples.ml", "ghci Examples.hs"]
