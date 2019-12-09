#!/bin/bash

exec 2>&1
set -e
set -x

# First, get our parent up to date
(cd /home/rlpowell/src/containers/stack ; ./build_container.sh)

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

sudo $CONTAINER_BIN kill build_container_mw_pather || true
sudo $CONTAINER_BIN rm build_container_mw_pather || true

sudo $CONTAINER_BIN build -t rlpowell/mw_pather . || {
  echo "Container build failed."
  exit 1
}

sudo $CONTAINER_BIN kill build_container_mw_pather || true
sudo $CONTAINER_BIN rm build_container_mw_pather || true
