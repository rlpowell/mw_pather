FROM haskell:7.8

# Initial Setup, Basic Package Installs
RUN DEBIAN_FRONTEND=noninteractive; apt-get update; apt-get upgrade -y; apt-get install --no-install-recommends -y vim zsh build-essential git openssh-client wget curl make ca-certificates libbz2-dev libcurl4-gnutls-dev libpcre3-dev; apt-get clean; rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# # Other Installs
# RUN mkdir -p /home/treed/bin/
# ADD http://stedolan.github.io/jq/download/linux64/jq /home/treed/bin/
# RUN chmod +x /home/treed/bin/jq
# 
# # Install Basic Configs
# WORKDIR /home/treed
# ENV HOME /home/treed
# ADD tmux.conf /home/treed/.tmux.conf
# ADD cvsignore /home/treed/.cvsignore
# ADD shellrc /home/treed/.shellrc
# ADD gitconfig /home/treed/.gitconfig
# ADD zshrc /home/treed/.zshrc
# ADD vim /home/treed/.vim
# ADD oh-my-zsh /home/treed/.oh-my-zsh
# ADD oh-my-zsh-custom /home/treed/.oh-my-zsh-custom
# 
# # Bootstrap vim
# ADD vimrc-plugins .vimrc
# RUN vim +PlugInstall +qa
# RUN mv .vimrc .vimrc-plugins
# ADD vimrc .vimrc
# 
# # Handle User
# RUN adduser --disabled-password -u 500 treed
# RUN chown -R treed:treed /home/treed
# RUN echo 'treed ALL=NOPASSWD: ALL' >> /etc/sudoers
# USER treed

RUN cabal update ; cabal install cabal-install ; cabal update ; cabal install cabal-install ; cabal update

RUN mkdir /srv ; cd /srv ; git clone https://github.com/valderman/haste-compiler.git ; mv haste-compiler haste ; mkdir /srv/haste/scratch ; cd /srv/haste ; cabal install ; haste-boot --force --local

RUN cd /srv/haste ; cabal install .

RUN cabal install --avoid-reinstalls regex-pcre missingh tagsoup

RUN haste-cabal install regex-base ; haste-cabal install --avoid-reinstalls regex-pcre

VOLUME /srv/haste/scratch

ENV TZ America/Los_Angeles

ENTRYPOINT ["zsh"]
