#!/bin/sh

if [ -z "$1" ]; then
  echo "Usage: lmw <word>"
  exit 1
fi

# --raw-control-chars makes 0x200b (zero-width space) display correctly.
~/bin/mw "$@" | exec less --raw-control-chars

