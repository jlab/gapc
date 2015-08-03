#!/bin/ksh

set -u

#NO_CONFIG_MF="foo"
#export NO_CONFIG_MF

DIR_BASE=../../..
GAPC=$DIR_BASE/gapc
GHC=ghc
MAKE=make
MAKEFLAGS="-I$DIR_BASE/testdata/gapc_filter"

TEMP=./temp
GRAMMAR=$DIR_BASE/testdata/grammar
LHS_DIR=..
#RTLIB=$DIR_BASE/rtlib
CPPFLAGS_EXTRA=" -I$DIR_BASE/testdata/gapc_filter "
#CPPFLAGS_EXTRA="-I$DIR_BASE -I$DIR_BASE/librna -I$DIR_BASE/testdata/gapc_filter -I$RTLIB"
LDLIBS_EXTRA=""
#LDLIBS_EXTRA="$DIR_BASE/librna/librna.a"
RUN_CPP_FLAGS=""

if [ -e $DIR_BASE/testdata/config.mf ]; then
  CONFIG_MF=$DIR_BASE/testdata/config.mf
else
  CONFIG_MF=$DIR_BASE/config.mf
fi

err_count=0
succ_count=0
failed=0

FILTER=.

if [ $# -ge 1 ]; then
  FILTER=$1
fi

TAG_FILTER=.
if [ $# == 2 ]; then
  TAG_FILTER=$2
fi


mkdir -p $TEMP
cd $TEMP

#echo include $CONFIG_MF > gapc_local.mf
#printf RT_LDLIBS=\\n $CONFIG_MF >> gapc_local.mf

. $DIR_BASE/testdata/tool.sh

. ../config

echo
. ../../stats.sh
