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
  save_dir="$dir"
else
  save_dir=~/.greek
fi

[ ! -d "$save_dir" ] && mkdir -p "$save_dir"

URL_PREFIX='http://media.merriam-webster.com/audio/prons/en/us/mp3'

audio_related_html=$(\
  wget -q -O - "http://www.m-w.com/dictionary/$1" | \
  grep '"play-pron"' | \
  tr ' ' '\n')

sound_files=$(\
  echo "$audio_related_html" | \
  grep data-file | \
  sed -r 's/.*data-file="([^"]*)".*/\1/')
sound_files=($sound_files)

sound_dirs=$(\
  echo "$audio_related_html" | \
  grep data-dir | \
  sed -r 's/.*data-dir="([^"]*)".*/\1/')
sound_dirs=($sound_dirs)

if [ ${#sound_files[@]} -eq 0 ]; then
  echo "No sounds found for \"$1\"." >&2
  exit 1
elif [ ${#sound_files[@]} -ne ${#sound_dirs[@]} ]; then
  echo "Error: parse failure" >&2
  exit 1
fi

sound_urls=()

for i in "${!sound_files[@]}"; do
  sound_url="$URL_PREFIX/${sound_dirs[$i]}/${sound_files[$i]}.mp3"
  sound_urls+=($sound_url)
done

# (Print array, separating elements by newlines.)
printf '%s\n' "${sound_urls[@]}" | sort -u | while read sound_url; do
  if ! wget -q "$sound_url" -P "$save_dir"; then
    echo "Error: couldn't download $sound_url"
    continue
  fi
  path="$save_dir/${sound_url##*/}"
  $player "$path"
done

