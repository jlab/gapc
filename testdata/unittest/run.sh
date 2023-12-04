#!/bin/ksh

if [ $# -lt 1 ]; then
  echo $0 UNITTEST \[UNITTEST...\]
  exit 1
fi
. ../log.sh

failed=0

for i in $*; do
  echo +------------------------------------------------------------------------------+
  log $i
  echo +------------------------------------------------------------------------------+
done

if [ $failed != 0 ]; then
  exit 1;
else
  exit 0;
fi

