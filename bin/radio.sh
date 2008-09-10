#!/bin/sh

if [ $# -gt 0 ] && [ $1 = '-e' ]; then
	XMMS_CMD="xmms -e $HOME/.radio_paradise3.m3u"
else
	XMMS_CMD="xmms $HOME/.radio_paradise3.m3u"
fi

#exec $XMMS_CMD
$XMMS_CMD &

