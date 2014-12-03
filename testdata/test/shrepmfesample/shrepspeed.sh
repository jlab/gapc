#!/bin/bash

set -e

if [ $# != 2 ]; then
  echo $0 n repeats
  exit 1
fi


RANDSEQ=../../../randseq/RandSeqMain
TSTIME=~/local/bin/tstime
SHREPMFESAMPLE=./main
SHAPEFILTER=../shapemfepfx/main

N=$1
REPEAT=$2

LC_ALL=C

print_time()
{
  t=`tr '\n' ' ' < ts.log | sed 's/^.\+real \(.\+\) s, user.\+rss \+\(.\+\) kb, vm.\+$/\1 \2/'`
  echo $t
}

for i in `seq $REPEAT`; do
  INP=`$RANDSEQ -l $N`
  echo Repeat: $i, n = $N
  echo $INP



  r=1000


  echo shapefilter -s
  $TSTIME $SHAPEFILTER -s $INP > log.a 2> ts.log
  a=`print_time`

#    echo shrepmfe -s -r $r
#    $TSTIME $SHREPMFESAMPLE -s -r $r $INP > log.b 2> ts.log
#    b=`print_time`


    echo N real_filter rss_filter real_sample rss_sample
    echo $N $a # $b

done
