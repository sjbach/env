#!/bin/bash
#
# Like a raw-mode bash read; blocks for input, but subsequently
# non-blocking, returning whatever input was available.

tty_state=$(stty -g)

function catch_sigint {
  # Clean up and propagate the SIGINT
  stty "$tty_state" 2>/dev/null
  trap - SIGINT
  kill -s INT $$
}

trap catch_sigint SIGINT

stty raw -echo

read -n 1 input

# Check for any remaining input - e.g. from arrow keys which
# emit multi-character sequences - but don't block.
while true; do
  # dd seems to manipulate /dev/tty in a way affecting the
  # parent process if I leave if= as stdin.
  char=$(dd if=/dev/tty bs=1 count=1 iflag=nonblock 2>/dev/null)
  if [ $? -ne 0 ]; then
    break
  fi
  input="${input}${char}"
done

stty "$tty_state"

echo "$input"

