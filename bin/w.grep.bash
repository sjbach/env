#!/bin/bash

db_dir=~/words

cd $db_dir

find . -type f ! -iname '*.wav' -print0 | \
  xargs -0 grep "$@" | \
    exec sed 's|^\./||'
