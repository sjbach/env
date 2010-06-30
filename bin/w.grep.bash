#!/bin/bash

cd ~/words
find . -type f ! -iname '*.wav' -print0 | xargs -0 grep "$@" | exec sed 's/^..//'
