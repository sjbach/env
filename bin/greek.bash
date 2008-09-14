#!/bin/bash
#
# Download the pronunciations for the given word and play them.


if [ $# -ne 1 ]; then
	echo "Incorrect arguments." >&2
	exit 2
fi

SOUND_DIR=~/.greek
NEWLINE='\
'

[ ! -d $SOUND_DIR ] && mkdir -p $SOUND_DIR

WAVS=$(\
  wget -q -O - "http://www.m-w.com/dictionary/$1" | \
  sed -n "/audio\.gif/s/>/$NEWLINE/pg" | \
  sed -rn "/popWin/s|^.*popWin\('([^']*)'\).*$|http://www\.m-w\.com/\1|gp"| \
  xargs -r wget -q -O - | \
  sed -n '/Click here/s/^.*<A HREF="\([^"]*\)">.*$/\1/gp')

if [ -z "$WAVS" ]; then
	echo "No sounds found for \"$1\"." >&2
	exit 1
fi

wget -q $WAVS -P $SOUND_DIR

for sound in $WAVS; do
	echo $SOUND_DIR/${sound##*/}
	aplay $SOUND_DIR/${sound##*/}
done

