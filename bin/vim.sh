#!/bin/sh
#
# When on an ssh session with X forwarding, sometimes connecting to the X
# display takes a long time and there's just no reason to even try.

# Avoid infinite recursion.
vim=`which -a vim | uniq | sed 1d | head -n1`
[ "$vim" ] || vim=`which vim`
[ "$vim" ] || vim=vim

DISPLAY='' exec $vim "$@"

