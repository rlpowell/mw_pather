FROM haskell:7.8

# Initial Setup, Basic Package Installs
RUN DEBIAN_FRONTEND=noninteractive; apt-get update; apt-get upgrade -y; apt-get install --no-install-recommends -y vim zsh build-essential git openssh-client wget curl make ca-certificates libbz2-dev libcurl4-gnutls-dev libpcre3-dev; apt-get clean; rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN cabal update ; cabal install cabal-install ; cabal update ; cabal install cabal-install ; cabal update

RUN mkdir /srv ; cd /srv ; git clone https://github.com/valderman/haste-compiler.git ; mv haste-compiler haste ; mkdir /srv/haste/scratch ; cd /srv/haste ; cabal install ; haste-boot --force --local

RUN cd /srv/haste ; cabal install .

RUN cabal install --avoid-reinstalls regex-pcre missingh tagsoup

RUN haste-cabal install regex-base ; haste-cabal install --avoid-reinstalls regex-pcre

VOLUME /srv/haste/scratch

ENV TZ America/Los_Angeles

ENTRYPOINT ["zsh"]
