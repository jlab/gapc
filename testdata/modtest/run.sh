#!/bin/ksh

# call for example:
# ksh run.sh ../../../adpc-tng-logs tab typecheck

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

mkdir -p $TEMP

if [ ! -d $REF ]; then
  echo Reference directory is no directory: $TEMP/$REF
  exit 1
fi

succ_count=0
err_count=0
failed=0

. ../log.sh

for j in ${*:2}; do
  cd $GRAMMAR
  mkdir -p $TEMP/$j
  for i in `ls * | grep -v '\(^core$\|\.orig$\)'`; do
    failed=0
    log_both $TEMP/$j/$i $PREFIX/$j $i
    if [ $failed != 0 ]; then
      err_count=$((err_count+1))
    fi
  done
done

module_errors=$err_count

err_count=0
cd $TEMP
for j in ${*:2}; do
  echo +------------------------------------------------------------------------------+
  failed=0
  l=`echo $REF/$j[0-9]* | tr ' ' '\n' | sed 's/\([0-9]\+\)/:\1/' |\
    sort -n -r -k 2 -t : | tr -d : | head -1`
  log diff -ur $l $j

  if [ $failed != 0 ]; then
    echo --++--FAIL--++--
    err_count=$((err_count+1))
  else
    echo OK
    succ_count=$((succ_count+1))
  fi
  echo +------------------------------------------------------------------------------+
done


echo +==============================================================================+
. ../../stats.sh
