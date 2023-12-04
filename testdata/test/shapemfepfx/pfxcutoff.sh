#!/bin/bash

set -e
set -u

if [ ! \( $# = 2 -o $# = 3 \) ] ; then
  echo $0 n repeats \[subwords\]
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

N=$1
REPEAT=$2

LC_ALL=C

print_time()
{
  t=`tr '\n' ' ' < ts.log | sed 's/^.\+real \(.\+\) s, user.\+rss \+\(.\+\) kb, vm.\+$/\1 \2/'`
  echo $t
}

echo N INITCUTOFF CUTOFF PROB-THRESH MISSING MSE REAL_A RSS_A REAL_B RSS_B SPEEDUP MEMUP


for i in `seq $REPEAT`; do
  echo -e "\t"shapemfepfx, $N, $REPEAT

  INP=`$RANDSEQ -l $N`

  echo -e "\t"$INP

  echo -e "\t"shapefilter, $N, $REPEAT, 0.000001
  $TSTIME $SHAPEFILTER $SUBWORDS -d  -t 0.000001 -p 0.01 $INP > cutoffref.log 2> ts.log
  a_time=`print_time`

  grep '\[' cutoffref.log > a.log
  a=`wc -l a.log | cut -d' ' -f1`


  for j in 0.00001 0.0001 0.001; do
    echo -e "\t"shapefilter, $N, $REPEAT, $j
    $TSTIME $SHAPEFILTER $SUBWORDS -d  -t $j -p 0.01 $INP > cutoff.log 2> ts.log
    b_time=`print_time`

    grep '\[' cutoff.log > b.log
    b=`wc -l b.log | cut -d' ' -f1`
    missing=$((a-b))


    mse=`awk '/\[/ { if (!f[FILENAME]) { f[FILENAME]=x+1; x++; }
           a=f[FILENAME];
           if (a==1) u_prob[$1]=$2; else v_prob[$1]=$2;
           sum[a]++; }
         END { for (i in u_prob) { b[i] = 1; }
               for (i in v_prob) { b[i] = 1; }
               count = sum[1] < sum[2] ? sum[2] : sum[1];
               for (i in b) { err_prob+=(u_prob[i]-v_prob[i])^2;  }
               print err_prob/count;
             }
               ' cutoffref.log cutoff.log`

   times=`echo $a_time $b_time | awk '{ print $1/$3, $2/$4; }' `


    echo $N 0.000001 $j 0.01 $missing $mse $a_time $b_time $times

  done
done
