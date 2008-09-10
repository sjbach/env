#!/bin/sh
# Send stdin to a temporary gvim buffer and stdout

if ! which pee >/dev/null 2>&1; then 
  echo "Install package 'moreutils'" >&2 
  exit 1
fi

GVIM_PARAMS="+set nowrap buftype=nofile bufhidden=hide"

if [ "$1" ] && [ ! "$1" == "-" ]; then
  title_string=`echo "$@" | sed 's/ /\\\\ /g'`
  "$@" | pee "gvim \"$GVIM_PARAMS titlestring=$title_string\" - 2>/dev/null" \
             cat
else
  exec pee "gvim \"$GVIM_PARAMS titlestring=stdin\" - 2>/dev/null" \
           cat
fi

