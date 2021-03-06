#!/bin/bash

shopt -s extglob

function usage {
  echo "${0##*/} <rev> [file]" >&2
  exit 0
}

function die {
  [ "$1" ] && echo "Error: $1" >&2
  usage
}

case "$1" in
  '')
    die "rev unspecified"
    ;;
  *([0-9]))
    rev=$1
    ;;
  -h|--help)
    usage
    ;;
  *)
    die "bad rev: $1"
    ;;
esac

file="$2"

if [ "$file" ]; then
  if ! egrep -q '://' <<< "$file" && \
     ! [ -e "$file" ]; then
    die "file does not exist: $file"
  fi
fi

svn log -v -r $rev "$file"
svn diff -c $rev "$file"

