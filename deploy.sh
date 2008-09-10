#!/bin/bash
#
# Create symlinks for environment files.

set -e
shopt -s extglob

die() {
  echo "Error: $1" >&2
  exit 1
}

symlink() {
  SRC="$1"
  DEST="$2"
  if [ -e "$DEST" ] && [ ! -L "$DEST" ]; then
    die "file exists: $DEST"
  fi

  echo "deploying: $DEST"
  rm -f "$DEST"
  ln -s "$SRC" "$DEST"
}

[ -d .git ] || die "run from git root"

DIR="$PWD"

mkdir -p ~/bin
cd bin
for file in *; do
  cd ~/bin
  symlink "$DIR/bin/$file" "${file%.*}"
  cd ~-
done

DIR="$DIR/dotfiles"

cd $DIR

find . -type f | sed 's|^\./||' | while read path; do
  dir=`sed -n '/\//s|/[^/]*$||p' <<< $path`
  file=${path##*/}

  if [ "$dir" ]; then
    mkdir -p ~/.$dir
    cd ~/.$dir
  else
    file=".$file"
    cd ~
  fi

  symlink "$DIR/$path" "$file"
done
 
