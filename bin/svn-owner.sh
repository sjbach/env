#!/bin/sh
#
# Figure out who added the given file to subversion.
#

if [ $# -ne 1 ]; then
  echo "Error: one file argument expected" >&2
  exit 1
elif [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
  echo "Usage: svn-owner [file]" >&2
  exit 2
elif [ ! -e "$1" ]; then
  echo "Error: file does not exist" >&2
  exit 3
fi

svn log -q "$1" | grep '^r[0-9]' | tail -n1 | cut -d' ' -f 3

