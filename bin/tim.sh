#!/bin/sh
# Send stdin to a temporary vim/gvim buffer

VIM_PARAMS="+set nowrap buftype=nofile bufhidden=hide"

if [ "$DISPLAY" ]; then
  cat 2>/dev/null | (
    if [ "$1" ] && [ ! "$1" == "-" ]; then
      title_string=`echo "$@" | sed 's/ /\\\\ /g'`
      "$@" | gvim "$VIM_PARAMS titlestring=$title_string" -
    else
      exec gvim "$VIM_PARAMS titlestring=<stdin>" -
    fi
  ) >/dev/null &
else
  if [ "$1" ] && [ ! "$1" == "-" ]; then
    title_string=`echo "$@" | sed 's/ /\\\\ /g'`
    "$@" | vim "$VIM_PARAMS titlestring=$title_string" -
  else
    exec vim "$VIM_PARAMS titlestring=<stdin>" -
  fi
fi

