/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define to 1 if you have the `asprintf' function. */
#define HAVE_ASPRINTF 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the `erand48' function. */
#define HAVE_ERAND48 1

/* Define to 1 if you have the <float.h> header file. */
#define HAVE_FLOAT_H 1

/* Define to 1 if you have the `floor' function. */
#define HAVE_FLOOR 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `gsl' library (-lgsl). */
#define HAVE_LIBGSL 1

/* Define to 1 if you have the `gslcblas' library (-lgslcblas). */
#define HAVE_LIBGSLCBLAS 1

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if your system has a GNU libc compatible `malloc' function, and
   to 0 otherwise. */
#define HAVE_MALLOC 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the <math.h> header file. */
#define HAVE_MATH_H 1

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `memset' function. */
#define HAVE_MEMSET 1

/* Define if OpenMP is enabled */
#define HAVE_OPENMP 1

/* Define to 1 if you have the `pow' function. */
#define HAVE_POW 1

/* Have PTHREAD_PRIO_INHERIT. */
#define HAVE_PTHREAD_PRIO_INHERIT 1

/* Define to 1 if your system has a GNU libc compatible `realloc' function,
   and to 0 otherwise. */
#define HAVE_REALLOC 1

/* Define to 1 if you have the `rint' function. */
#define HAVE_RINT 1

/* Define to 1 if you have the `sqrt' function. */
#define HAVE_SQRT 1

/* Define to 1 if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define to 1 if stdbool.h conforms to C99. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strrchr' function. */
#define HAVE_STRRCHR 1

/* Define to 1 if you have the `strstr' function. */
#define HAVE_STRSTR 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `vasprintf' function. */
#define HAVE_VASPRINTF 1

/* Define to 1 if the system has the type `_Bool'. */
#define HAVE__BOOL 1

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#define LT_OBJDIR ".libs/"

/* Name of package */
#define PACKAGE "ViennaRNA"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "rna@tbi.univie.ac.at"

/* Define to the full name of this package. */
#define PACKAGE_NAME "ViennaRNA"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "ViennaRNA 2.4.17"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "ViennaRNA"

/* Define to the home page for this package. */
#define PACKAGE_URL "http://www.tbi.univie.ac.at/RNA"

/* Define to the version of this package. */
#define PACKAGE_VERSION "2.4.17"

/* Define to necessary symbol if this constant uses a non-standard name on
   your system. */
/* #undef PTHREAD_CREATE_JOINABLE */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* only for gcc */
#define UNUSED __attribute__ ((unused))

/* Use floating point precision in partition function computations */
/* #undef USE_FLOAT_PF */

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Version number of package */
#define VERSION "2.4.17"

/* Use Hash for non-redundant sampling data structure */
/* #undef VRNA_NR_SAMPLING_HASH */

/* Use MPFR for non-redundant sampling data structure operations */
/* #undef VRNA_NR_SAMPLING_MPFR */

/* Warn upon usage of deprecated symbols */
/* #undef VRNA_WARN_DEPRECATED */

/* Do not use colors for TTY output */
/* #undef VRNA_WITHOUT_TTY_COLORS */

/* Use Boustrophedon scheme for stochastic backtracking */
#define VRNA_WITH_BOUSTROPHEDON 1

/* Use GNU Scientific Library */
#define VRNA_WITH_GSL 1

/* Add JSON support for input and output functions */
#define VRNA_WITH_JSON_SUPPORT 1

/* Use pthreads for parallel input processing */
#define VRNA_WITH_PTHREADS 1

/* use AVX 512 implementations */
#define VRNA_WITH_SIMD_AVX512 1

/* use SSE 4.1 implementations */
#define VRNA_WITH_SIMD_SSE41 1

/* Compute z-scores for RNALfold */
#undef VRNA_WITH_SVM

/* Include C-library Unit tests */
/* #undef WITH_CHECK */

/* Analyse{Dists,Seqs} */
/* #undef WITH_CLUSTER */

/* Define if using the dmalloc debugging malloc package */
/* #undef WITH_DMALLOC */

/* Create the perl interface to RNAlib */
#define WITH_PERL_INTERFACE 1

/* Create the python2 interface to RNAlib */
#define WITH_PYTHON2_INTERFACE 1

/* Create the Python3 interface to RNAlib */
#define WITH_PYTHON3_INTERFACE 1

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to rpl_malloc if the replacement function should be used. */
/* #undef malloc */

/* Define to rpl_realloc if the replacement function should be used. */
/* #undef realloc */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */
