#!/bin/sh
# Resize (bytes) the given images.

for file in "$@"; do
	[ -f "$file" ] || continue
	file "$file" | sed 's/.*://' | grep -q image || continue

	before=`du -h "$file" | sed 's/[[:space:]].*//'`

	gimp --no-interface --batch - &>/dev/null <<-EOF
		(define image (car (gimp-file-load 1 "$file" "$file")))
		(define drawable (car (gimp-image-active-drawable image)))
		(gimp-file-save 1 image drawable "$file" "$file")
		(gimp-quit 0)
		EOF

	after=`du -h "$file" | sed 's/[[:space:]].*//'`

	if [ "$before" = "$after" ]; then
		echo "$file: (no change)"
	else
		echo "$file: $before --> $after"
	fi
done

