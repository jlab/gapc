
# configure support 07/2015

AC_INIT([gapc], [pr1.0], [tgatter@cebitec.uni-bielefeld.de])

AC_CONFIG_FILES([config.mf])


AC_CONFIG_MACRO_DIR([m4])

AC_CANONICAL_BUILD
AC_CANONICAL_HOST


## get shell
AC_PATH_PROGS(SHELL, [bash sh ksh93 ksh])


## compile and optimize flags
AC_ARG_ENABLE([--debug], [Set --debug to disable optimization and set debug flags],
[
CFLAGS+=" -g "
CXXFLAGS+=" -g "
], [
CFLAGS+=" -O3 "
CXXFLAGS+=" -O3 "
])

## get compiler
AC_PROG_CXX
AC_PROG_CC

## other programms
AC_PROG_INSTALL
AC_PROG_SED

## Test for c11 or c99
AC_LANG([C])
AX_CHECK_COMPILE_FLAG([-std=c11], [
  CFLAGS+=" -std=c11"
], [
	AX_CHECK_COMPILE_FLAG([-std=c99], [
	  CFLAGS+=" -std=c99"
	], [
		  echo "C compiler cannot compile C11 or C99 code"
		  exit -1
	])
])


##fast-math
AX_CHECK_COMPILE_FLAG([-ffast-math], [
  FAST_MATH=" -ffast-math "
], [
  FAST_MATH=" "
  echo "-ffast-math not supported by compiler"
])
AC_SUBST(FAST_MATH)

## test for Position Independednd Code, sets PIC_FLAGS
GAP_PIC_FLAGS


AC_LANG([C++])
AX_CHECK_COMPILE_FLAG([-std=c++11], [
  CXXFLAGS+=" -std=c++11"
])

## test additional flags
AX_CHECK_COMPILE_FLAG([-D_XOPEN_SOURCE=500],
    [CXXFLAGS+=" -D_XOPEN_SOURCE=500"])

AX_CHECK_COMPILE_FLAG([-MMD -MP],
    [CXXFLAGS+=" -MMD -MP"])

AX_CHECK_COMPILE_FLAG([-Wall -Wnon-virtual-dtor -Wno-unused-variable -Wno-parentheses],
    [CXXFLAGS+=" -Wall -Wnon-virtual-dtor -Wno-unused-variable -Wno-parentheses"])

## openmp option
AX_OPENMP([AC_SUBST(OPENMP_CXXFLAGS)])

## test for shared linking option based on OS
GAP_OS_FLAGS


AC_PATH_PROG([FLEX], [flex])
if test "x$FLEX" = "x"; then
	echo "flex is needed to compile GapC."
	exit -1
fi

AC_PATH_PROG([BISON], [bison])
if test "x$BISON" = "x"; then
	echo "bison is needed to compile GapC."
	exit -1
fi
 

AC_PATH_PROG([HG], [hg])

## bison version switch
AX_PROG_BISON_VERSION([3.0],
[BISON_VERSION_FLAG=" -DBISONNEW "],
[BISON_VERSION_FLAG=""])
AC_SUBST(BISON_VERSION_FLAG)

## test for boost # BOOST_CPPFLAGS, BOOST_LDFLAGS
AX_BOOST_BASE([1.36],, [
 echo "Boost version 1.36 or newer needed to compile GapC"
 exit -1
])

#BOOST_PROGRAM_OPTIONS_LIB
AX_BOOST_PROGRAM_OPTIONS

#BOOST_UNIT_TEST_FRAMEWORK_LIB
AX_BOOST_UNIT_TEST_FRAMEWORK

## GSL , GSL_LIBS, GSL_CFLAGS
AX_PATH_GSL([1.0],[],
[
 echo "GSL needed to compile GapC"
 exit -1
])


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_C_CONST
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset strerror])

AC_OUTPUT










