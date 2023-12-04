
AC_DEFUN([GAP_OS_FLAGS],
[

case "$host_os" in
*-macos*|darwin*|rhapsody*)
	SO_SUFFIX=".dylib"
	SHARED_FLAGS="-dynamiclib "
	INSTALL_SHARED_FLAG="-install_name @rpath/"
	;;
cygwin*|mingw*)
	SO_SUFFIX=".dll"
	SHARED_FLAGS="-shared "
	;;
*)
	SO_SUFFIX=".so"
	SHARED_FLAGS="-shared "
	;;
esac

AC_SUBST(SO_SUFFIX)
AC_SUBST(SHARED_FLAGS)
AC_SUBST(INSTALL_SHARED_FLAG)

])
