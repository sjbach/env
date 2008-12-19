#!/bin/bash
#
# Grep through archived command history.
#

# Get results, reverse, prepend line numbers
grep "$@" -- ~/.histfile ~/.hists/* | tac | nl | \
#
# Insert line numbers for better intra-day sorting
sed -r 's/([0-9]+) *([^:]*):(.*)/\2:\1:\3/' | \
#
# Pretty print
sed -r 's%.*\.hists/(.*).(histfile|bash_history):%\1:%' | \
#
# Put more recent lines first like this:
#   year, month, day, then implicitly by the inserted line number
# Do this now so that the unique sort prefers more recent lines.
sort --key 1.7,1.8nr --key 1.1,1.3Mr --key 1.4,1.5nr | \
#
# Remove duplicates
sort --stable -u -t ':' --key 3 | \
#
# Put more recent lines first again (it gets screwed up)
sort --key 1.7,1.8nr --key 1.1,1.3Mr --key 1.4,1.5nr | \
#
# Remove the inserted line numbers.
sed -r 's/:[0-9]+:/:/'

