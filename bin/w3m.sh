#!/bin/sh
#
# Start w3m on the bookmark page by default.

# Avoid infinite recursion.
W3M=`which -a w3m | sed 1d | head -n1`
[ "$W3M" ] || W3M=`which w3m`
[ "$W3M" ] || W3M=w3m

[ "$@" ] || exec $W3M -B
exec $W3M "$@"
