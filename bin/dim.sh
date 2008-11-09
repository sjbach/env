#!/bin/sh
# Send stdin or output from running arguments to a temporary gvim buffer as
# well as stdout.

[ "$DISPLAY" ] || exec tim "$@"

if ! which pee >/dev/null 2>&1; then 
  echo "Install package 'moreutils'" >&2 
  exit 1
fi

GVIM_PARAMS="+set nowrap buftype=nofile bufhidden=hide"

if [ "$1" ] && [ "$1" != "-" ]; then
  TITLE=`echo "$@" | sed 's/ /\\\\ /g'`
  "$@" | pee "gvim \"$GVIM_PARAMS titlestring=$TITLE\" - 2>/dev/null" \
             cat
else
  TITLE='<stdin>'
  exec pee "gvim \"$GVIM_PARAMS titlestring=$TITLE\" - 2>/dev/null" \
           cat
fi

