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

  echo shapefilter
  $TSTIME $SHAPEFILTER $INP > filter.log 2> ts.log
  a=`print_time`

  for r in 1000 2000 4000 5000 10000; do

    echo shrepmfe -r $r
    $TSTIME $SHREPMFESAMPLE -r $r $INP > sample.log 2> ts.log
    b=`print_time`


    R="$a $b $r" awk '/\[/ { if (!f[FILENAME]) { f[FILENAME]=x+1; x++; }
           a=f[FILENAME];
           if (a==1) u_prob[$1]=$2; else v_prob[$1]=$2;
           if (a==1) u_mfe[$1]=$3; else v_mfe[$1]=$3;
           sum[a]++; }
         END { for (i in u_prob) { b[i] = 1; }
               for (i in v_prob) { b[i] = 1; }
               count = sum[1] < sum[2] ? sum[2] : sum[1];
               for (i in b) { err_prob+=(u_prob[i]-v_prob[i])^2;  }
               for (i in b) { err_mfe+=(u_mfe[i]-v_mfe[i])^2;  }
               for (i in b) { err_mfe_w+=u_prob[i]*((u_mfe[i]-v_mfe[i])^2); }
               for (i in b) { if (u_prob[i]>=0.01) { err_mfe_1pc+=(u_mfe[i]-v_mfe[i])^2; count_1pc++; } }
               print "filter_real filter_rss sample_real sample_rss samples MSE_prob MSE_mfe MSE_weighted_mfe MSE_mfe_geq1pc";
               print ENVIRON["R"], err_prob/count, err_mfe/count, err_mfe_w/count, err_mfe_1pc/count_1pc ;
             }
               ' filter.log sample.log

  done
done
