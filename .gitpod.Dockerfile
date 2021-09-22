FROM gitpod/workspace-base

RUN sudo apt-get update
RUN sudo curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
RUN ghcup install ghc 8.10.1 && ghcup install cabal 3.2.0.0 && ghcup set ghc 8.10.1 && ghcup set cabal 3.2.0.0
 
