#!/bin/sh
# Send stdin or output from running arguments to a temporary vim/gvim buffer

VIM_PARAMS="+set nowrap buftype=nofile bufhidden=hide"

if [ "$DISPLAY" ]; then
  VIM=gvim
  style=display
else
  VIM=vim
  style=terminal
fi

if [ "$1" ] && [ "$1" != "-" ]; then
  TITLE=`echo "$@" | sed 's/ /\\\\ /g'`
  style="$style subcommand"
else
  TITLE='<stdin>'
  style="$style stdin"
fi

run_vim () {
  [ "$1" ] && exec $VIM "$VIM_PARAMS titlestring=$TITLE" -
  $VIM "$VIM_PARAMS titlestring=$TITLE" -
}

case $style in
  *stdin)
    run_vim exec
    ;;
  "terminal subcommand")
    "$@" | run_vim
    ;;
  "display subcommand")
    cat 2>/dev/null | (
      "$@" | run_vim
    ) >/dev/null &
    ;;
esac

