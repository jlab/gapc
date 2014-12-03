#!/bin/sh

awk ' { count++; sum+=$6; a[count]=$6; }
      END { mean=sum/count; for (i in a) { x+=(a[i]-mean)^2; }
            print "Sampling var: ", x/count, "Standard deviation: ", sqrt(x/count); }'
