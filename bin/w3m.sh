#!/bin/bash
#
# Start w3m on the bookmark page by default.

# Avoid infinite recursion.
W3M=`which -a w3m | sed 1d | head -n1`
[ "$W3M" ] || W3M=`which w3m`
[ "$W3M" ] || W3M=w3m

if [ "$1" ]; then
  if [ -e "$1" ] || \
     [[ "$1" == -* ]] || \
     echo "$*" | grep -q '\.' ; then
    exec $W3M "$@"
  else
    terms=`echo "$*" | sed 's/ /+/g'`
    url="http://www.google.com/search?q=$terms"
    exec $W3M "$url"
  fi
elif [ ! -e ~/.w3m/bookmark.html ]; then
  exec $W3M
else
  exec $W3M -B
fi

