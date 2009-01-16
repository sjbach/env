#!/bin/sh
# Send stdin or output from executing arguments to a temporary gvim buffer as
# well as stdout.

[ "$DISPLAY" ] || exec tim "$@"

if ! which pee >/dev/null 2>&1 ; then
  echo "Install package 'moreutils'" >&2
  exit 1
fi

params="+set nowrap buftype=nofile bufhidden=hide"

if [ "$1" ] && [ "$1" != "-" ]; then
  title=`echo "$@" | sed 's/ /\\\\ /g'`
  "$@" | pee "gvim \"$params titlestring=$title\" - 2>/dev/null" \
             cat
else
  title='<stdin>'
  exec pee "gvim \"$params titlestring=$title\" - 2>/dev/null" \
           cat
fi

