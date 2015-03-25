#!/bin/bash
#
# Download the pronunciations for the given word and play them.

function usage {
  echo "usage: ${0##*/} <word> [dir]" >&2
  exit 1
}

case $# in
  0) echo "Error: no words given." >&2
     usage
     ;;
  1) ;;
  2) dir="$2" ;;
  *) echo "Error: too many arguments." >&2
     usage
     ;;
esac

sound="$1"

if [ "$dir" ]; then
  sound_dir="$dir"
else
  sound_dir=~/.greek
fi

[ ! -d "$sound_dir" ] && mkdir -p "$sound_dir"

sound_names=$(\
  wget -q -O - "http://www.m-w.com/dictionary/$1" | \
  grep -v '\<wow_entry\>' | \
  grep '\<au(' | \
  sed -r "s/au\('([^']+)/\n--\1--\n/g" | \
  sed -rn '/^--/s/--([^-]+).*/\1/p')

if [ -z "$sound_names" ]; then
  echo "No sounds found for \"$1\"." >&2
  exit 1
fi

#    wget -q -O - "http://www.merriam-webster.com/cgi-bin/audio.pl?$sound=$word" | \
sound_urls=$(\
  for sound in $sound_names; do
    wget -q -O - "http://www.merriam-webster.com/audio.php?file=$sound" | \
    grep -i '<EMBED' | sed -r 's/.*SRC="([^"]+)".*/\1/'
  done)

wget -q $sound_urls -P "$sound_dir"

for url in $sound_urls; do
  path="$sound_dir/${url##*/}"
  mv "$path" "$path".wav
  echo "$path".wav
  aplay "$path".wav
done

