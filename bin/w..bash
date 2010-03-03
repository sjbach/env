#!/bin/bash
#
# Display information about a given word and play pronunciations.
# Stores data to $db_dir for faster subsequent lookup.

db_dir=~/words

if [ "$1" = "-f" ]; then
  force=yep
  shift
fi

word="$1"
word_dir="$db_dir/$word"

function die {
  echo "$1" >&2
  exit 1
}

function usage {
  echo "usage: ${0##*/} <word>" >&2
  exit 1
}

function get_def {
  dictionary="$1"
  word="$2"

  dict -d "$1" "$2" 2>/dev/null | sed '1,2d'
}

if ! [ "$word" ] || \
     [[ "$word" = "-h" ]] || \
     [[ "$word" = "--help" ]]; then
  usage
fi

dict_dbs=$(dict --dbs | sed 1d | sed -r 's/^ //; s/[[:space:]]+.*//')

if ! [ -d "$word_dir" ] ; then
  if ! dict "$word" >/dev/null 2>&1 && [ -z "$force" ]; then
    die "$word is unknown"
  fi

  mkdir -p "$word_dir"

  thes -q "$word" > "$word_dir/thes"
  mw "$word" > "$word_dir/mw"
  greek "$word" "$word_dir"

  for dict in $dict_dbs; do
    get_def "$dict" "$word" > "$word_dir/$dict"
  done

  date > "$word_dir/date"
fi

( for wav in "$word_dir"/*.wav; do test -e "$wav" && aplay -q "$wav"; done ) &
( if [ -s "$word_dir/mw" ]; then
    cat "$word_dir/mw"
    echo
    echo "==============================================="
    echo
  fi
  if [ -s "$word_dir/wn" ]; then
    cat "$word_dir/wn"
    echo
    echo "==============================================="
    echo
  fi
  if [ -s "$word_dir/thes" ]; then
    cat "$word_dir/thes"
    echo
    echo "==============================================="
    echo
  fi

  for dict in $dict_dbs; do
    if [ "$dict" != wn ] && [ -s "$word_dir/$dict" ]; then
      cat "$word_dir/$dict"
      echo
      echo "==============================================="
      echo
    fi
  done
  ) | less

