#!/bin/sh

if [ -z "$1" ]; then
  echo "Usage: lmw <word>"
  exit 1
fi

~/bin/mw "$@" | exec less

