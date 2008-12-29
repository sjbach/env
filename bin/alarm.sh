#!/bin/sh
# Three beeps

(
  for i in 1 2 3; do
    /bin/echo -en '\a'
    sleep 0.3 
  done 2>/dev/null
) &

