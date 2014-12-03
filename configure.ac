#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT([gapc], [0.42], [gsauthof@techfak.uni-bielefeld.de])
AC_CONFIG_SRCDIR([expr_fwd.hh])
AC_CONFIG_HEADER([config.h])

# Checks for programs.

AC_PROG_CXX
#AC_PROG_CC
AC_PROG_CC_C99

AX_COMPILER_VENDOR
#ax_cxx_check_flag


AC_PROG_YACC
AX_PROG_BISON(,[AC_MSG_ERROR([need Bison parser generator])])


AC_PROG_LEX
#AC_PROG_INSTALL
AC_PROG_MAKE_SET

#CHECK_GNU_MAKE()

AC_ARG_VAR([LEX],[The scanner generator implementation to use.])

AS_IF([ test -n "`echo \"${LEX}\" | grep '^flex$'`" || test -n "`echo \"${LEX}\" | grep '/flex$'`" ],
      [ AC_MSG_CHECKING([flex version])
        versionstring="`${LEX} -V | sed 's/\./ /g;s/[[^0-9 ]]//g;s/^ *//'`"
        major="`echo ${versionstring} | cut -f 1 -d ' '`"
        minor="`echo ${versionstring} | cut -f 2 -d ' '`"
        micro="`echo ${versionstring} | cut -f 3 -d ' '`"
        if test -n "${major}" && test  -n "${minor}" && test -n "${micro}"
        then
          ok=1
          test $major -lt 2 && ok=0
          if test $ok = 1 && test $major -eq 2
          then
            test $minor -lt 5 && ok=0
            if test $ok = 1 && test $minor -eq 5
            then
              test $micro -lt 34 && ok=0
            fi
          fi
        else
          ok=0
        fi
        AS_IF([ test $ok = 1 ],
                [ AC_MSG_RESULT([ok]) ],
              [ AC_MSG_RESULT([fail])
                AC_MSG_ERROR([need flex version >= 2.5.34])])
      ])




# Checks for libraries.
# FIXME: Replace `main' with a function in `-lboost_program_options':
##AC_CHECK_LIB([boost_program_options], [main])
# FIXME: Replace `main' with a function in `-lboost_unit_test_framework':
##AC_CHECK_LIB([boost_unit_test_framework], [main])
# FIXME: Replace `main' with a function in `-lfl':
#AC_CHECK_LIB([fl], [main])
# FIXME: Replace `main' with a function in `-lm':
AC_CHECK_LIB([m], [log])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([inttypes.h libintl.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset strerror])

AX_BOOST_BASE([1.34])
AX_BOOST_PROGRAM_OPTIONS
AX_BOOST_UNIT_TEST_FRAMEWORK

AC_CONFIG_FILES([ac_makefile])

AC_OUTPUT
