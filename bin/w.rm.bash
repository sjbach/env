#!/bin/bash

db_dir=~/words

if [ "$1" ]; then
  rm -r $db_dir/"$1"
fi

