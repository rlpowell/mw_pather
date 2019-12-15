#!/bin/bash

exec 2>&1
set -e
set -x

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

./build_container.sh

sudo -u rlpowell $CONTAINER_BIN kill mw_pather || true
sudo -u rlpowell $CONTAINER_BIN rm mw_pather || true

# Run the container to generate the javascript client side, and
# generate and run the executable/Haskell server side
sudo -u rlpowell $CONTAINER_BIN run --userns=keep-id -it --name mw_pather \
  -p 192.168.123.133:24601:24601 \
  -v /home/rlpowell:/home/rlpowell:rw \
  "$@" rlpowell/mw_pather bash -x /tmp/container_run_init.sh
