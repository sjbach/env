#!/bin/sh
# Ignore files in .svn directories.

exec find "$@" | grep -v '\.svn'

