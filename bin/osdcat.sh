#!/bin/sh

if ! which osd_cat >/dev/null 2>&1; then
  echo "aptitude install xosd-bin" >&2
  exit 1
fi

echo "$*" | \
  osd_cat -o -190 -A right -p bottom \
          -f -*-*-*-*-*-*-34-*-*-*-*-*-*-* -c green -s 5 &
