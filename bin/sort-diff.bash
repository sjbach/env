#!/bin/bash
#
# Diffs the two given files, which are first sorted.

if ! which tempfile >/dev/null 2>&1; then
  echo '`tempfile` binary required.' >&2
  exit 1
fi

file1="$1"
file2="$2"

sorted_file_1=$(tempfile)
sorted_file_2=$(tempfile)

sort "$file1" > "$sorted_file_1"
sort "$file2" > "$sorted_file_2"

diff -dur "$sorted_file_1" "$sorted_file_2"

rm -f "$sorted1" "$sorted2"

