#!/bin/bash

word="$1"
word_dir=~/words/"$word"
date_file=/tmp/date.$$

if [ ! -d "$word_dir" ]; then
  echo "no word: $word" >&2
  exit 1
fi

if [ -f "$word_dir"/carded ]; then
  carded=yep
else
  carded=
fi

mv "$word_dir"/date $date_file

set -e

w.rm "$word"
w. "$word"

mv $date_file "$word_dir"/date
if [ "$carded" ]; then
  touch "$word_dir"/carded
fi

