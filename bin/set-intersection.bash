#!/bin/bash
#
# Performs a set-intersection on the two given files, which are first sorted
# and deduplicated.

if ! which tempfile >/dev/null 2>&1; then
  echo '`tempfile` binary required.' >&2
  exit 1
fi

file1="$1"
file2="$2"

sorted_file_1=$(tempfile)
sorted_file_2=$(tempfile)

sort -u "$file1" > "$sorted_file_1"
sort -u "$file2" > "$sorted_file_2"

comm -12 "$sorted_file_1" "$sorted_file_2"

rm -f "$sorted_file_1" "$sorted_file_2"

