#!/bin/ksh

set -u

NO_CONFIG_MF="foo"
export NO_CONFIG_MF

GAPC=../../../gapc
GHC=ghc
MAKE=make
MAKEFLAGS="-I../../.. -I../../gapc_filter"

TEMP=./temp
GRAMMAR=../../grammar
LHS_DIR=..
RTLIB=../../../rtlib
CPPFLAGS_EXTRA="-I../../.. -I../../../librna -I.. -I../../gapc_filter"
LDLIBS_EXTRA=""
RUN_CPP_FLAGS=""

if [ -e ../../config.mf ]; then
  CONFIG_MF=config.mf
else
  CONFIG_MF=config/generic.mf
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

echo include $CONFIG_MF > gapc_local.mf
printf RT_LDLIBS=\\n $CONFIG_MF >> gapc_local.mf

. ../../tool.sh

. ../config

echo
. ../../stats.sh
