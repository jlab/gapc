#!/bin/ksh

# call for example:
# ksh gen.sh ../../adpc-tng-logs tab typecheck

set -u

if [ $# -lt 2 ]; then
  echo $0 DATA-BASE TOOL-ONE \[MORE_TOOLS...\]
  exit 1
fi


GRAMMAR=../grammar
WORKDIR=`pwd`
TEMP=$WORKDIR/temp
REF=$1
PREFIX=../..

if [ ! -d $REF ]; then
  echo Reference directory is no directory: $REF
  exit 1
fi

. ../log.sh

failed=0

for j in ${*:2}; do
  echo +------------------------------------------------------------------------------+
  echo Generating for tool $j:
  l=`echo $REF/$j[0-9]* | tr ' ' '\n' | sed 's/^.*'$j'\([0-9]\+\).*$/\1/' |\
    sort -n -r | head -1`
  new=$REF/$j$(($l+1))
  echo   new dir: $new

  mkdir $new
  cd ..
  cd grammar
  for i in `ls * | grep -v '\(^core$\|\.orig$\)'`; do
    #../$j $i 2> $new/$i
    log_both $new/$i ../../$j $i
  done
  cd ../modtest
  echo +------------------------------------------------------------------------------+
done

echo failed: $failed
