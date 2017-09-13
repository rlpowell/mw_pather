FROM rlpowell/stack

RUN mkdir /home/rlpowell/src || true
RUN mkdir /home/rlpowell/src/mw_pather || true

USER rlpowell
WORKDIR /home/rlpowell/src/mw_pather

# RUN dnf -y update
RUN sudo dnf -vv -y install bzip2

COPY docker_run_init.sh /tmp/

# Get haste
RUN cd /tmp/ ; wget http://haste-lang.org/downloads/ghc-7.10/haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2 ; tar -xf haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2
RUN cd /tmp/ ; tar -xf haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2

# Install haste
RUN cd /tmp/haste-compiler/ ; sudo ./install.sh

# Setup stack (~/.local/, ~/.stack/, /tmp/mw_pather/.stack-work)
RUN mkdir /tmp/mw_pather/
COPY mw-pather.cabal mw_pather.hs stack.yaml /tmp/mw_pather/
RUN cd /tmp/mw_pather/ ; stack setup
RUN cd /tmp/mw_pather/ ; stack install
