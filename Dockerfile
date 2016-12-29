FROM rlpowell/stack

RUN mkdir /home/rlpowell/src || true
RUN mkdir /home/rlpowell/src/mw_pather || true

USER rlpowell
WORKDIR /home/rlpowell/src/mw_pather

# RUN dnf -y update
RUN sudo dnf -y install bzip2

COPY docker_run_init.sh /tmp/
