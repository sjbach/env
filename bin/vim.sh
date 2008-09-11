#!/bin/sh
#
# When on an ssh session with X forwarding, sometimes connecting to the X
# display takes a long time and there's just no reason to even try.

# Avoid infinite recursion.
VIM=`which -a vim | sed 1d | head -n1`

# (Just in case this script isn't in the path.)
[ "$VIM" ] || VIM=vim

DISPLAY= exec $VIM "$@"

