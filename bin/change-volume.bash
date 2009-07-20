#!/bin/bash
#
# Minimal alsamixer interface for more effective use
# in screen over ssh:
#
# k or up-arrow   -- increase volume 10%
# j or down-arrow -- decrease volume 10%
#

function mixer_mod() {
  if [ "$1" = up ]; then
    change=10%+
  else
    change=10%-
  fi

  if ! amixer -q set Master $change 2>/dev/null && \
     ! amixer -q set PCM $change 2>/dev/null ; then
    echo "No Master or PCM" >&2
    exit 1
  fi
}
  
while read data; do
  case "$data" in
    k*|*[A) mixer_mod up ;;
    j*|*[B) mixer_mod down ;;
  esac
done

