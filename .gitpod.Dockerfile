FROM gitpod/workspace-full

# Install haskell toolchain and linter
RUN sudo apt-get update \
    && sudo apt-get install -y \
        haskell-platform \
        hlint \
    && sudo rm -rf /var/lib/apt/lists/*