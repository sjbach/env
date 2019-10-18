#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: looped-keypress-run <COMMAND ...>"
  exit 1
fi

while true; do
  echo "Waiting for keypress to run \`$*\`"
  if ! raw-read >/dev/null; then
    echo "Error: raw-read failed"
    exit 2
  fi
  "$@"
done

