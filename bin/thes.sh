#!/bin/sh

if ! which lynx >/dev/null 2>&1; then 
  echo "Install lynx" >&2 
  exit 1
elif ! which html2text >/dev/null 2>&1; then 
  echo "Install html2text" >&2 
  exit 2
fi

BROWSER="lynx -source"
HTML2TEXT="html2text -style compact"

term=`echo "$1" | sed 's/ /+/g'`

#URL="http://www.m-w.com/cgi-bin/thesaurus?book=Thesaurus&va=$term"
URL="http://thesaurus.reference.com/search?q=$term"

if [ "$1" ]; then
#    $BROWSER $URL | sed '1,/<!-- begin content -->/d; /<!-- end content -->/,$d' | $HTML2TEXT | $PAGER
#    $BROWSER $URL | sed '1,/<!-- Content -->/d; /SPONSORED LINKS/,$d' | $HTML2TEXT | $PAGER
#    $BROWSER $URL | sed '1,/<!-- Content -->/d' | sed '/SPONSORED LINKS/,$d' #| $HTML2TEXT | $PAGER
    $BROWSER $URL  | $HTML2TEXT | less
else
    echo "Usage: $0 <word>" >&2
    exit 1
fi

