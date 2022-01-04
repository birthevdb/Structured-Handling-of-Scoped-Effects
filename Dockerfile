# Get ocaml version 4.12
ARG OCAML_VERSION="4.12"
FROM ocaml/opam:debian-ocaml-${OCAML_VERSION}
ARG OCAML_VERSION

# Install Glasgow Haskell Compiler
RUN sudo apt-get update && sudo apt-get install -y ghc

# Clone repo to get files
RUN git clone https://github.com/birthevdb/Structured-Handling-of-Scoped-Effects.git
WORKDIR Structured-Handling-of-Scoped-Effects

# Run the examples
CMD ocaml examples.ml && ghci Examples.hs
