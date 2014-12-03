set -e
set -u
cd /vol/gapc/src/adpc-tng_slave
hg pull -u
make -j 8
make install
