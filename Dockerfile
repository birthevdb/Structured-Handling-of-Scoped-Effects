FROM haskell:8.10
RUN cabal update
# && cabal install <al je dependencies>
RUN git clone https://github.com/birthevdb/Structured-Handling-of-Scoped-Effects.git
WORKDIR Structured-Handling-of-Scoped-Effects
CMD ["ghci Exmaples.hs"]
