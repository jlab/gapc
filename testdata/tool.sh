. ../../log.sh

set +u
GAPC_EXTRA=$GAPC_EXTRA
set -u

IGNORE_INSTANCE="no"

build_cpp()
{
  if [ $IGNORE_INSTANCE == "yes" ]; then
    INST=""
  else
    INST="-i $3"
  fi
  log ${GAPC} ${GAPC_EXTRA} $1 -o $2.cc $INST
  log ${MAKE} ${MAKEFLAGS} -f $2.mf CPPFLAGS_EXTRA\="${CPPFLAGS_EXTRA}" LDLIBS_EXTRA\="${LDLIBS_EXTRA}"
}

build_haskell()
{
  log ${GHC} --make $LHS_DIR/$1 -i$LHS_DIR -hidir ./GHC -odir ./GHC -o $2
}

run_cpp()
{
  log2 $1.$2.$4.out $1.$2.$4.err ./$1 $RUN_CPP_FLAGS $3
}

run_haskell()
{
  log2 $1.$2.$4.hs.out $1.$2.$4.hs.err ./$1 $2 $3
}


set +u
EPSILON=$EPSILON
set -u
cmp_fp_output()
{
  if [ "$EPSILON" == "" ]; then
    EPSILON=0.1
  fi
  out1=$1.$3.$4.out
  out2=$2.$3.$4.hs.out
  log1 u sed '/Answer/d' $out1
  log_filter u a tr -d "\n "
  log1 x sed '/Answer/d' $out2
  log_filter x b tr -d "\n "
  log ../../fp_eq `cat a` `cat b` $EPSILON
}


set +u
CPP_FILTER=$CPP_FILTER
OWN_CMP_OUTPUT=$OWN_CMP_OUTPUT
set -u
cmp_output()
{
  if [ "$OWN_CMP_OUTPUT" != "" ]; then
    $OWN_CMP_OUTPUT $1 $2 $3 $4
    return
  fi

  if [ "$CPP_FILTER" != "" ]; then
    log $CPP_FILTER $1.$3.$4.out
  fi
  log1 s sort $1.$3.$4.out
  log1 a sed '/Answer/d' s
  log1 d sort $2.$3.$4.hs.out
  log_filter d b tr -d "'\"'"
  log diff -u -w -B a b
}


check_eq()
{
  if [ $# != 5 ]; then
    echo Not enough parameters to check_eq
    exit 1
  fi
  if [[ `echo $1$3 | grep $FILTER` != $1$3  ]]; then
    return
  fi
  if [[ `echo $5 | grep $TAG_FILTER` != $5  ]]; then
    return
  fi
  ##############################################################################
  # Workaround for 1s timestamp resolution filesystems
  ##############################################################################
  # e.g. on ext3 and a fast machine it is possible, that new generated deps are
  # not rebuild if two consecutive check_eq calls
  # are executed in a 1 s window ...
  sleep 1
  ##############################################################################

  cpp_base=${1%%.*}
  lhs_base=${2%%.*}
  echo +------------------------------------------------------------------------------+
  echo Checking $cpp_base \($3, $5\) ...
  failed=0
  temp=$failed
  build_cpp $GRAMMAR/$1 $cpp_base $3
  build_haskell $2 $lhs_base
  run_cpp $cpp_base $3 "$4" $5
  run_haskell $lhs_base $3 "$4" $5
  cmp_output $cpp_base $lhs_base $3 $5
  if [ $temp != $failed ]; then
    echo --++--FAIL--++--
    err_count=$((err_count+1))
    #exit 1
  else
    echo OK
    succ_count=$((succ_count+1))
  fi
  echo +------------------------------------------------------------------------------+

}

