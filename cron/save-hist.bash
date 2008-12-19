#!/bin/bash
#
# Archive .histfile files.
#

cd ~

current_hist=.histfile

[ -f $current_hist ] || exit 0

storage_dir=.hists
prev_hist=`ls -ct $storage_dir/* 2>/dev/null | head -n1`

if [ -z "$prev_hist" ] || ! diff $current_hist $prev_hist >/dev/null ; then
  mkdir -p $storage_dir
  cp -p $current_hist $storage_dir/`date +%b%d-%y`.histfile
fi

