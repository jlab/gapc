#!/bin/bash

set -e
set -u

if [ ! \( $# = 2 -o $# = 3 \) ] ; then
  echo $0 windowsize repeats \[subwords\]
  exit 1
fi

if [ $# == 3 ]; then
  SUBWORDS="-s"
else
  SUBWORDS=""
fi


RANDSEQ=../../../randseq/RandSeqMain
TSTIME=~/local/bin/tstime
SHAPEFILTER=./main

WINDOW=$1
REPEAT=$2

LC_ALL=C

print_time()
{
  t=`tr '\n' ' ' < ts.log | sed 's/^.\+real \(.\+\) s, user.\+rss \+\(.\+\) kb, vm.\+$/\1 \2/'`
  echo $t
}

echo N WINDOWSIZE CUTOFF PROBTHRESH REAL RSS

for i in `seq $REPEAT`; do
  for N in `seq 100 100 4000`; do

  echo -e "\t"windowtest, $N, $i

  INP=`$RANDSEQ -l $N`

  echo -e "\t"$INP

  echo -e "\t"shapefilter, $N, $REPEAT, 0.000001
  $TSTIME $SHAPEFILTER $SUBWORDS -w $WINDOW -d -t 0.000001 -p 0.01 $INP > /dev/null 2> ts.log
  a_time=`print_time`

  echo $N $WINDOW 0.000001 0.01 $a_time


  done
done
