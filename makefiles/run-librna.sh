#!/bin/sh

set -e
set -u

if [ $# -lt 1 ]; then
  echo call: $0 PREFIX
  exit 1
fi

PREFIX="$1"
SHARE="$PREFIX/share/gapc"
EXAMPLES="$SHARE/examples"

SED=`grep SED config.mf | tr -d ' ' | cut -d= -f2`

SO_SUFFIX=`grep SO_SUFFIX config.mf | tr -d ' ' | cut -d= -f2`
SYSTEM_SUFFIX=`grep SYSTEM_SUFFIX config.mf | tr -d ' ' | cut -d= -f2`

if [ x$SO_SUFFIX = x ]; then
  SO_SUFFIX=".so"
fi

set +e
#~ install -d $PREFIX/bin
set -e
#~ install -d $PREFIX/include/rtlib
install -d $PREFIX/include/librna
install -d "$SHARE"
install -d "$SHARE"/librna
#~ install -d "$EXAMPLES"
set +e
install -d $PREFIX/lib
set -e

#~ install -m 755 gapc $PREFIX/bin

#~ for i in rtlib/*; do
  #~ install -m 644 $i $PREFIX/include/rtlib
#~ done

install -m 644 librna/rnalib.h $PREFIX/include/librna

#install -m 644 config.mf "$SHARE"

CONF_PREFIX=`grep 'char prefix' prefix.cc | $SED 's/^[^"]\+"\([^"]\+\)".*$/\1/'`

#~ $SED -e 's@^PREFIX[ ?]*=.*$@PREFIX='"$CONF_PREFIX"'@' \
  #~ -e 's/^#CXXFLAGS_EXTRA/CXXFLAGS_EXTRA/' \
  #~ config.mf > "$SHARE"/config$SYSTEM_SUFFIX.mf
#~ chmod 644 "$SHARE"/config$SYSTEM_SUFFIX.mf

install -m 644 librna/rnalib.c "$SHARE"/librna

install -m 644 librna/librna$SO_SUFFIX "$PREFIX"/lib
install -m 644 librna/librnafast$SO_SUFFIX "$PREFIX"/lib

for i in librna/vienna/*.par
do
  install -m 644 $i "$SHARE"/librna
done

#~ for i in grammar/elm.gap grammar/adpf.gap paraltest/adpf_filter.hh \
  #~ grammar/adpf_nonamb.gap paraltest/pf_filter.hh paraltest/nonamb_answer.hh \
  #~ grammar/nussinov2.gap \
  #~ grammar/affinelocsim2.gap; do
  #~ install -m 644 $i "$EXAMPLES"
#~ done


