#!/bin/bash
#
# Trivial wrapper around emacsclient

exec emacsclient --alternate-editor="" -c "$@"
