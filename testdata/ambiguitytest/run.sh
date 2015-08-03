#!/bin/ksh

set -u

#NO_CONFIG_MF="foo"
#export NO_CONFIG_MF

GAPC=../../../gapc
GHC=ghc
MAKE=make
MAKEFLAGS=-I../../..

TEMP=./temp
GRAMMAR=../../grammar
LHS_DIR=..
#RTLIB=../../../rtlib
#CPPFLAGS_EXTRA="-I../../.. -I../../../librna -I../../../rtlib -O -DNDEBUG"
#LDLIBS_EXTRA=""
#RUN_CPP_FLAGS=""

CPPFLAGS_EXTRA=" -I../../../testdata/gapc_filter "
LDLIBS_EXTRA=""
RUN_CPP_FLAGS=""

KSH="ksh"

SED=`cat ../../config.mf | grep "^SED" | cut -d "=" -f 2`

if [ -e ../../config.mf ]; then
  CONFIG_MF=config.mf
else
  CONFIG_MF=config/generic.mf
fi

if [ $# -ge 3 ]; then
	KSH=$3
fi

err_count=0
succ_count=0
failed=0

FILTER=.

if [ $# == 2 ]; then
  FILTER=$2
fi

REF=../../../../adpc-tng-logs/runs
if [ $# -ge 1 ]; then
  REF=$1
fi

mkdir -p $TEMP

if [ ! -d $REF ]; then
  echo Reference directory is no directory: $REF
  exit 1
fi

cd $TEMP

. ../../tool.sh

#echo include $CONFIG_MF > gapc_local.mf
#printf RT_LDLIBS=\\n $CONFIG_MF >> gapc_local.mf
#printf RT_LDLIBS04=\\n $CONFIG_MF >> gapc_local.mf

check_ambiguity() {
	#$1 = gap file name, $2 = instance, $3 = testSuffixname
	if [[ `echo $1$2$3 | grep $FILTER` != $1$2$3  ]]; then
		return
	fi

	# work around 1 sec timestamp filesystems ... WTF?!?
	#~ sleep 1

	echo +------------------------------------------------------------------------------+
	failed=0
	temp=$failed
	testname=${1%%.*}.$2.$3
	
	#run gapc
	rungapconly $GRAMMAR/$1 $testname $2
	
	#perform comparison
	cmp_ambigutiy $testname $REF
	
	if [ $temp != $failed ]; then
		echo --++--FAIL--++--
		err_count=$((err_count+1))
	else
		echo OK
		succ_count=$((succ_count+1))
	fi
	echo +------------------------------------------------------------------------------+
}

check_ambiguity_errors() {
	#$1 = gap file name, $2 = instance, $3 = testSuffixname
	if [[ `echo $1$2$3 | grep $FILTER` != $1$2$3  ]]; then
		return
	fi

	# work around 1 sec timestamp filesystems ... WTF?!?
	#~ sleep 1

	echo +------------------------------------------------------------------------------+
	failed=0
	temp=$failed
	testname=${1%%.*}.$2.$3

	#run gapc
	rungapErrors $GRAMMAR/$1 $testname $2
	
	#perform comparison
	cmp_ambigutiyErrors $testname $REF
	
	if [ $temp != $failed ]; then
		echo --++--FAIL--++--
		err_count=$((err_count+1))
	else
		echo OK
		succ_count=$((succ_count+1))
	fi
	echo +------------------------------------------------------------------------------+
}

check_specialization() {
	#$1 = gap file name, $2 = instance, $3 = testSuffixname
	if [[ `echo $1$2$3 | grep $FILTER` != $1$2$3  ]]; then
		return
	fi

	# work around 1 sec timestamp filesystems ... WTF?!?
	#~ sleep 1

	echo +------------------------------------------------------------------------------+
	failed=0
	temp=$failed
	testname=${1%%.*}.$2.$3
	
	#run gapc
	rungapconly $GRAMMAR/$1 $testname $2
	
	#perform comparison
	cmp_specialization $testname $REF
	
	if [ $temp != $failed ]; then
		echo --++--FAIL--++--
		err_count=$((err_count+1))
	else
		echo OK
		succ_count=$((succ_count+1))
	fi
	echo +------------------------------------------------------------------------------+
}

rungapconly() {
	if [ $IGNORE_INSTANCE == "yes" ]; then
		INST=""
	else
		INST="-i $3"
	fi
	log ${GAPC} ${GAPC_EXTRA} $1 -o $2.cc $INST
}

rungapErrors() {
	if [ $IGNORE_INSTANCE == "yes" ]; then
		INST=""
	else
		INST="-i $3"
	fi
	${GAPC} ${GAPC_EXTRA} $1 -o $2.cc $INST > $2.record 2>&1
}

cmp_ambigutiy() {
	cat $2/$1.cfg | $SED -r "s|/\*.*?\*/||g" | $SED '/^$/d' > $1.puretruth
	cat $1.cfg | $SED -r "s|/\*.*?\*/||g" | $SED '/^$/d' > $1.pureresult
	log diff -u -w -B $1.puretruth $1.pureresult
}

cmp_ambigutiyErrors() {
	log diff -u -w -B $2/$1.record $1.record
}

cmp_specialization() {
	log diff -u -w -B $1.gap $2/$1.gap
}


. ../config

. ../../stats.sh

