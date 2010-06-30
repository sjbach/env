#!/bin/bash

db_dir=~/words

word="$1"
word_dir=$db_dir/"$word"
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

