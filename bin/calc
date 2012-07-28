#!/bin/bash
#
# Make bc useful

default_scale=5

if [ -z "$1" ]; then
  bc -q -i <(echo "scale=$default_scale")
else
  echo "scale=$default_scale; $@" | bc -q
fi
