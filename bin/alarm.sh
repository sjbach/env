#!/bin/sh
# Three beeps

msg="$1"
if [ -z "$msg" ]; then
  msg="DONE"
fi

(
  if [ "$DISPLAY" ]; then
    osdcat "$msg"
  fi
  for i in 1 2 3; do
    /bin/echo -en '\a'
    sleep 0.3
  done 2>/dev/null
) &

