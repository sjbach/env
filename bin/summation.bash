#!/bin/bash

exec awk '{ sum += $1 } END { print sum }'
