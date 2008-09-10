#!/bin/bash
#
# Generate a report of incremental diffs on the given file or dir.
#

shopt -s extglob

function usage {
  echo "${0##*/} <file> [count]" >&2
}

function die {
  [ "$1" ] && echo "Error: $1" >&2
  exit 1
}

case "$1" in
  -h|--help|'')
    usage
    die
    ;;
  -v|--verbose)
    VERBOSE=yep
    shift
    ;;
esac

NAME="$1"
LIMIT="$2"

if ! [ -e "$NAME" ]; then
  die "$NAME does not exist"
elif svn info "$NAME" 2>&1 | grep -q 'Not a versioned resource'; then
  die "$NAME not under version control"
elif [ "$LIMIT" ] && [[ "$LIMIT" != *([0-9]) ]]; then
  die "Malformed limit: $LIMIT"
fi

[ -z "$LIMIT" ] && LIMIT=10

i=0
svn log -q "$NAME" 2>/dev/null | grep '^r[[:digit:]]' | while read line; do
  rev=`cut -d' ' -f1 <<< $line`
  rev=${rev#r}

  if [ "$VERBOSE" ]; then
    svn log -r $rev "$NAME" 2>/dev/null
  else
    echo "--------------------------------------------------------------------"
    echo "$line"
    echo "--------------------------------------------------------------------"
  fi

  svn diff -c $rev "$NAME"

  i=$((i + 1))
  [ $i -ge $LIMIT ] && break
done

