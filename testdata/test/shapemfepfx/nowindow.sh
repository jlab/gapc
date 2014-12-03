#!/bin/sh

./nowindow $* |\
  sed -e 's/^( ( \([^ ]\+\) , ( (\([^,]\+\)[^)]\+) , \([^ ]\+\)[^,]\+, \([^ ]\+\).*$/\1;\2;\3;\4/g ; /Answer/d' |\
  awk -F\; '{ sum+=$3; a[$1]=$3; aa[$1]=$2; aaa[$1]=$4; } END { for (i in a) printf("%s;%s;%s;%s\n", i, aa[i], a[i]/sum, aaa[i]); }'
