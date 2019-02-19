#!/bin/bash

exec 2>&1
set -e
set -x

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

sudo $CONTAINER_BIN kill mw_pather || true
sudo $CONTAINER_BIN rm mw_pather || true
#sudo $CONTAINER_BIN rmi rlpowell/mw_pather
sudo $CONTAINER_BIN build -t rlpowell/mw_pather . || {
  echo "Container build failed."
  exit 1
}

# Run the container to generate the javascript client side, and
# generate and run the executable/Haskell server side
sudo $CONTAINER_BIN run -it --name mw_pather --log-driver syslog --log-opt tag=mw_pather \
  -p 192.168.123.133:24601:24601 \
  -v volume--home--rlpowell:/home/rlpowell \
  -v /home/rlpowell/src/mw_pather/:/home/rlpowell/src/mw_pather/:rw \
  -v /home/rlpowell/public_html/:/home/rlpowell/public_html:rw \
  "$@" rlpowell/mw_pather bash -x /tmp/container_run_init.sh
