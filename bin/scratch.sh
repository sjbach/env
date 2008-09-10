#!/bin/sh
# Create and open a scratch file with a unique name

SCRATCH_DIR=~/.vim-scratch

mkdir -p $SCRATCH_DIR
cd $SCRATCH_DIR

tmp_file=`TMPDIR="$SCRATCH_DIR" tempfile -p 'vim-' -s '.scratch'`

exec gvim "$tmp_file"

