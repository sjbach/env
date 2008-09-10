#!/bin/sh

BROWSER="/usr/bin/lynx -source"
#WEBSITE="http://www.m-w.com/cgi-bin/thesaurus?book=Thesaurus&va=$1"
WEBSITE="http://thesaurus.reference.com/search?q=$1"
HTML2TEXT="/usr/bin/html2text -style compact"

if [ "$1" ]; then
    #${BROWSER} ${WEBSITE} | sed '1,/<!-- begin content -->/d; /<!-- end content -->/,$d' | ${HTML2TEXT} | ${PAGER}
    #${BROWSER} ${WEBSITE} | sed '1,/<!-- Content -->/d; /SPONSORED LINKS/,$d' | ${HTML2TEXT} | ${PAGER}
    #${BROWSER} ${WEBSITE} | sed '1,/<!-- Content -->/d' | sed '/SPONSORED LINKS/,$d' #| ${HTML2TEXT} | ${PAGER}
    ${BROWSER} ${WEBSITE}  | ${HTML2TEXT} | less
else
    echo "Usage: $0 word" >&2
    exit 1
fi

