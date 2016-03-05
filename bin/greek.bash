#!/bin/bash
#
# Download and play Merriam-Webster pronunciations for the given word.

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

player=$(which mplayer || which mpv || which play)
if [ -z "$player" ]; then
  echo "No mp3 player installed" >&2
  exit 1
fi

sound="$1"

if [ "$dir" ]; then
  sound_dir="$dir"
else
  sound_dir=~/.greek
fi

[ ! -d "$sound_dir" ] && mkdir -p "$sound_dir"

URL_PREFIX='http://media.merriam-webster.com/audio/prons/en/us/mp3/'

sound_urls=$(\
  wget -q -O - "http://www.m-w.com/dictionary/$1" | \
  grep '"play-pron"' | \
  tr ' ' '\n' | \
  grep data-file | \
  sed -r 's/.*data-file="([^"]*)".*/\1/' | \
  sed -r 's|^(.).*|\1/&.mp3|' | \
  sed "s!^!$URL_PREFIX!" | \
  sort -u)

if [ -z "$sound_urls" ]; then
  echo "No sounds found for \"$1\"." >&2
  exit 1
fi

wget -q $sound_urls -P "$sound_dir"

for url in $sound_urls; do
  path="$sound_dir/${url##*/}"
  $player "$path"
done

