#!/bin/bash
# Grep through archived command history.

# Get results
grep "$@" -- ~/.histfile ~/.hists/* | \
# Pretty print
sed -r 's/.*\.hists\/(.*).(histfile|bash_history):/\1:/' | \
# Remove duplicates
sort -u -t ':' --key 2 | sort -r -M

