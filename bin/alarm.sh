#!/bin/sh
# Three beeps

msg="$1"
if [ -z "$msg" ]; then
  msg="DONE"
fi

(
#  if [ "$DISPLAY" ]; then
#    osdcat "$msg"
#  fi
  afplay '/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/test/audiodata/pluck-pcm32.wav' 2>/dev/null
#  for i in 1 2 3; do
#    /bin/echo -en '\a'
#    sleep 0.3
#  done 2>/dev/null
) &

