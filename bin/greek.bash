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

if [ "$dir" ]; then
  sound_dir="$dir"
else
  sound_dir=~/.greek
fi

[ ! -d "$sound_dir" ] && mkdir -p "$sound_dir"

WAVS=$(\
  wget -q -O - "http://www.m-w.com/dictionary/$1" | \
  sed -n '/audio\.gif/s/>/\n/pg' | \
  sed -rn "/popWin/s|^.*popWin\('([^']*)'\).*$|http://www\.m-w\.com/\1|gp"| \
  xargs -r wget -q -O - | \
  sed -rn '/Click here/s/^.*<A HREF="([^"]*)">.*$/\1/gp')

if [ -z "$WAVS" ]; then
	echo "No sounds found for \"$1\"." >&2
	exit 1
fi

wget -q $WAVS -P "$sound_dir"

for sound in $WAVS; do
	echo "$sound_dir"/${sound##*/}
	aplay "$sound_dir"/${sound##*/}
done

