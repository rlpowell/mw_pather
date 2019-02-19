#!/bin/bash

exec 2>&1
set -x

CONTAINER_BIN=${CONTAINER_BIN:-$(which podman)}
CONTAINER_BIN=${CONTAINER_BIN:-$(which docker)}

sudo $CONTAINER_BIN stop --time=30 mw_pather
sudo $CONTAINER_BIN kill mw_pather
sudo $CONTAINER_BIN rm mw_pather
