
check_exit()
{
  if [ $1 != 0 ]; then failed=$(($failed + 1)); fi
}

log()
{
  echo $@
  "$@"
  check_exit $?
}

log1()
{
  echo ${*:2} . $1
  ${*:2} > $1
  check_exit $?
}

log2()
{
  echo ${*:3} . $1 .. $2
  ${*:3} > $1 2> $2
  check_exit $?
}

log_both()
{
  echo ${*:2} . $1
  ${*:2} > $1 2>&1
  check_exit $?
}

log_filter()
{
  echo ${*:3} : $1 . $2
  ${*:3} < $1 > $2
  check_exit $?
}

