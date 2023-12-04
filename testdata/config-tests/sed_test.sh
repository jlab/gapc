set -e
set -u


foo=`echo 'const char prefix[] = "/vol/gapc";' | $1 's/^[^"]\+"\([^"]\+\)".*$/\1/'`

if [ "$foo" = /vol/gapc ]; then
  exit 0
else
  exit 23
fi

