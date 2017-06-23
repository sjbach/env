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

REPOS_PATH="$PWD"

# bin scripts
#
scripts=`ls bin/`
mkdir -p ~/bin
cd ~/bin
for file in $scripts; do
  symlink "$REPOS_PATH/bin/$file" "${file%.*}"
done

# dotfiles
#
cd "$REPOS_PATH/dotfiles"
find . -type f | sed 's/^..//' | while read path; do
  dir=`sed -n '/\//s|/[^/]*$||p' <<< $path`  # (empty if at pwd)
  file=`basename $path`

  if [ "$dir" ]; then
    mkdir -p ~/."$dir"
    cd ~/."$dir"
  else
    file=".$file"
    cd ~
  fi

  symlink "$REPOS_PATH/dotfiles/$path" "$file"
done

# crons
#
if [ "$1" = "do-cron" ]; then
  cd "$REPOS_PATH/cron"
  for file in *; do
    fields=`grep '^# cron:' "$file" | sed 's/.*cron: //'`
    [ "$fields" = "no-auto-deploy" ] && continue

    line="$fields $REPOS_PATH/cron/$file"
    if crontab -l | grep -q "$file"; then
      echo "updating cron: $file"
      crontab -l | sed "/$file/s|.*|$line|" | crontab -
    else
      echo "adding cron: $file"
      ( crontab -l ; echo "$line" ) | crontab -
    fi
  done
fi

