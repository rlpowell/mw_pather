FROM rlpowell/stack

RUN mkdir /home/rlpowell/src || true
RUN mkdir /home/rlpowell/src/mw_pather || true

USER rlpowell
WORKDIR /home/rlpowell/src/mw_pather
ENV PATH="/home/rlpowell/.local/bin/:${PATH}"

# RUN dnf -y update
RUN sudo dnf -vv -y install bzip2 ncurses-devel

# Get haste
RUN cd /tmp/ ; wget http://haste-lang.org/downloads/ghc-7.10/haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2 ; tar -xf haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2
RUN cd /tmp/ ; tar -xf haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2

# Install haste
RUN cd /tmp/haste-compiler/ ; sudo ./install.sh

# Update stack
RUN stack upgrade
RUN stack --version
RUN stack --resolver lts-6.26 setup
RUN stack path --bin-path

COPY container_run_init.sh /tmp/
