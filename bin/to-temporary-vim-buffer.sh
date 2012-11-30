#!/bin/sh
# Send stdin or output from running arguments to a temporary vim/gvim buffer
# TODO: add arguments to an env variable
# TODO: within Vim, add a binding to refresh the output by
#       calling system() with args from the variable

if [ "$DISPLAY" ]; then
  vim=gvim
  style=display
else
  vim=vim
  style=terminal
fi

if [ "$1" ] && [ "$1" != "-" ]; then
  title=`echo "$@" | sed 's/ /\\\\ /g'`
  style="$style subcommand"
else
  title='<stdin>'
  style="$style stdin"
fi

run_vim () {
  params="+set nowrap buftype=nofile bufhidden=hide titlestring=$title"
  if echo "$title" | grep -q 'diff'; then
    # hack
    params="$params filetype=diff"
  fi
  [ "$1" ] && exec $vim "$params" -
  $vim "$params" -
}

case $style in
  *stdin)
    run_vim do_exec
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

