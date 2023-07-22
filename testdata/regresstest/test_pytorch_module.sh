#!/bin/ksh

if [ $# == 0 ]; then
  echo "Usage: ./test_pytorch_module.sh path/to/Truth/ [batched]"
  exit 0
fi

set -u

GAPC=../../../gapc
MAKE="make"
PYTHON="python3"
PY_TEST="../test_pytorch_module.py"
ALIGNMENTS_GAP="../../grammar_pytorch/alignments_tensor.gap"

# path/to/Truth
TRUTH_DIR=$1

BATCHED_ARG=""
BATCHED_SUFFIX=""

# if 2nd argument is provided, assume that
# batched test is supposed to be executed
if [ $# == 2 ]; then
  BATCHED_ARG="--batched"
  BATCHED_SUFFIX="_batched"
  ALIGNMENTS_GAP="../../grammar_pytorch/alignments_tensor_batched.gap"
fi

TEMP="./temp"
mkdir -p $TEMP
cd $TEMP

err_count=0
succ_count=0

execute_test() {
  # args: alignment mode, number of derivative, module name, [batched]
  alignment_mode=$1
  n_derivative=$2
  module_name=$3

  # generate code for Pytorch module
  $GAPC -i $alignment_mode$BATCHED_SUFFIX"_deriv_"$n_derivative --derivative \
  $n_derivative --as-torchmod $module_name $ALIGNMENTS_GAP

  # generate interface and setup.py file
  $MAKE -f out.mf

  # install the Pytorch module
  $MAKE -f out.mf install

  # execute python test script for the current aligment mode
  $PYTHON $PY_TEST $BATCHED_ARG $alignment_mode $n_derivative $TRUTH_DIR

  #get exit code of python script
  exit_code=$?

  if [ $exit_code != 0 ]; then
    err_count=$((err_count+1))
  else
    succ_count=$((succ_count+1))
  fi

  # clean up
  $MAKE -f out.mf clean
  rm -f out.*
}

# execute tests for all supported alignment modes/algorithms and derivatives
execute_test "nw" 1 "nw_gapc"$BATCHED_SUFFIX
execute_test "nw" 2 "nw_gapc"$BATCHED_SUFFIX

execute_test "gotoh" 1 "gotoh_gapc"$BATCHED_SUFFIX
execute_test "gotoh" 2 "gotoh_gapc"$BATCHED_SUFFIX

# check if/how many tests failed and exit accordingly
. ../../stats.sh
