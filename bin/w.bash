#!/bin/bash
#
# Display information about a given word and play pronunciations.
# Stores data to $db_dir for faster subsequent lookup.

db_dir=~/words
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

if ! [ "$word" ] || \
     [[ "$word" = "-h" ]] || \
     [[ "$word" = "--help" ]]; then
  usage
fi

if ! [ -d "$word_dir" ]; then
  if ! dict "$word" >/dev/null 2>&1; then
    die "$word is unknown"
  fi

  mkdir -p "$word_dir"

  dict "$word" > "$word_dir/dict"
  thes "$word" > "$word_dir/thes"
  greek "$word" "$word_dir"
  date > "$word_dir/date"
fi

( for wav in "$word_dir"/*.wav; do test -e "$wav" && aplay -q "$wav"; done ) &
( cat "$word_dir/dict"
  echo
  echo "==============================================="
  echo
  cat "$word_dir/thes" ) | less

