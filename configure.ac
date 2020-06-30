#              
# Configure template for OpenCOBOL                                         -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2001-2009 Keisuke Nishida
# Copyright (C) 2007-2009 Roger While
#
# This file is part of OpenCOBOL.
#
#
# The OpenCOBOL compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 2 of the
# License, or (at your option) any later version.
#
# OpenCOBOL is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with OpenCOBOL.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ(2.59)

AC_INIT([opensource COBOL],[1.5.2],[ws-opensource-cobol-contact@osscons.jp],[opensource-cobol-1.5.2J])
AC_CONFIG_SRCDIR([libcob.h])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_MACRO_DIR([m4])

# Note for SUN Solaris (gcc)
# export/setenv CFLAGS "-m64 -mptr64"
# export/setenv LDFLAGS "-m64 -mptr64 -L/usr/local/lib/sparcv9"
# ./configure --libdir=/usr/local/lib/sparcv9
#
# Hack for AIX 64 bit (gcc)
# Required -
# export/setenv CFLAGS=-maix64
# export/setenv LDFLAGS=-maix64

if echo $CFLAGS | grep 'aix64' 1>/dev/null 2>&1
then
	if test -f /usr/ccs/bin/ar
	then
		AR="/usr/ccs/bin/ar -X64"
	else
		AR="ar -X64"
	fi
	NM="/usr/ccs/bin/nm -X64 -B"
fi

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])
AM_MAINTAINER_MODE

# Autoheader templates
AH_TEMPLATE([WITH_DB], [Compile with the Berkeley DB library])
AH_TEMPLATE([USE_DB41], [Use Berkeley DB library new features])
AH_TEMPLATE([WITH_INDEX_EXTFH], [Compile with an external ISAM handler])
AH_TEMPLATE([WITH_SEQRA_EXTFH], [Compile with an external SEQ/RAN handler])
AH_TEMPLATE([WITH_CISAM], [Use CISAM as ISAM handler])
AH_TEMPLATE([WITH_DISAM], [Use DISAM as ISAM handler])
AH_TEMPLATE([WITH_VBISAM], [Use VBISAM as ISAM handler])
# AH_TEMPLATE([WITH_LFS64], [Compile with large file system 64])
AH_TEMPLATE([COB_EXPERIMENTAL], [Enable experimemental code (Developers only!)])
AH_TEMPLATE([COB_PARAM_CHECK], [Enable CALL parameter checking])
AH_TEMPLATE([I18N_UTF8], [Enable UTF-8 support])
AH_TEMPLATE([COB_EXPORT_DYN], [Compile/link option for exporting symbols])
AH_TEMPLATE([COB_PIC_FLAGS], [Compile/link option for PIC code])
AH_TEMPLATE([COB_SHARED_OPT], [Compile/link option for shared code])
AH_TEMPLATE([COB_STRIP_CMD], [Strip command])
AH_TEMPLATE([USE_LIBDL], [Use system dynamic loader])
AH_TEMPLATE([PATCH_LEVEL], [Define a patch level])
AH_TEMPLATE([WITH_VARSEQ], [Define variable sequential file format])
# AH_TEMPLATE([DLLHIDDEN], [Has visibility attribute])
AH_TEMPLATE([HAVE_PSIGN_OPT], [Has -Wno-pointer-sign])
AH_TEMPLATE([HAVE_ATTRIBUTE_ALIGNED], [Has __attribute__((aligned))])
AH_TEMPLATE([HAVE_TIMEZONE], [Has timezone variable])
AH_TEMPLATE([COB_EXEEXT], [Executable extension])
AH_TEMPLATE([COB_EXTRA_FLAGS], [Extra Compiler Flags])
AH_TEMPLATE([__USE_STRING_INLINES], [Compiler optimization])
AH_TEMPLATE([COB_HAS_INLINE], [Can use inline keyword])
AH_TEMPLATE([COB_NO_SELFOPEN], [Can not dlopen self])
AH_TEMPLATE([COB_STRFTIME], [Can use strftime for timezone])
AH_TEMPLATE([COB_LI_IS_LL], [long int is long long])
AH_TEMPLATE([HAVE_COLOR_SET], [curses has color_set function])

AC_DEFINE(__USE_STRING_INLINES)

LIBCOB_LIBS="-lm"
LIBCOB_CFLAGS="$lt_prog_compiler_pic"

save_libs="$LIBS"

AC_PROG_CC(gcc xlc cc)
AC_PROG_CPP
AC_PROG_EGREP

# Configure options.

COB_CC=""
AC_ARG_WITH(cc,
  [  --with-cc=<cc>          (OpenCOBOL) specify the C compiler used by cobc],
  [ COB_CC=$withval ], [ COB_CC=$CC] )

AC_ARG_WITH(seqra-extfh,
  [  --with-seqra-extfh      (OpenCOBOL) Use external SEQ/RAN file handler],
  [ if test "$with_seqra_extfh" = "yes"
    then
	AC_DEFINE(WITH_SEQRA_EXTFH)
    fi ], [])

AC_ARG_WITH(cisam,
  [  --with-cisam            (OpenCOBOL) Use CISAM for ISAM I/O],
  [ if test "$with_cisam" = "yes"
    then
	AC_CHECK_HEADERS([isam.h], , AC_MSG_ERROR(isam.h is required for CISAM))
	AC_CHECK_LIB([ifisam], [isopen], , AC_MSG_ERROR(libifisam is required for CISAM), [ifisamx])
	AC_DEFINE(WITH_CISAM)
	LIBCOB_LIBS="$LIBCOB_LIBS -lifisam -lifisamx"
	WITH_ISAM_DEFINED=yes
    fi ], [])

if test "$with_cisam" != "yes"
then
 AC_ARG_WITH(disam,
  [  --with-disam            (OpenCOBOL) Use DISAM for ISAM I/O],
  [ if test "$with_disam" = "yes"
    then
	AC_CHECK_HEADERS([disam.h], , AC_MSG_ERROR(disam.h is required for DISAM))
	AC_CHECK_LIB([disam], [isopen], , AC_MSG_ERROR(libdisam is required for DISAM))
	AC_DEFINE(WITH_DISAM)
	LIBCOB_LIBS="$LIBCOB_LIBS -ldisam"
	WITH_ISAM_DEFINED=yes
    fi ], [])
fi

if test "$with_cisam" != "yes" -a "$with_disam" != "yes"
then
AC_ARG_WITH(vbisam,
  [  --with-vbisam           (OpenCOBOL) Use VBISAM for ISAM I/O],
  [ if test "$with_vbisam" = "yes"
    then
	AC_CHECK_HEADERS([vbisam.h], , AC_MSG_ERROR(vbisam.h is required for VBISAM))
	AC_CHECK_LIB([vbisam], [isopen], , AC_MSG_ERROR(libvbisam is required for VBISAM))
	AC_DEFINE(WITH_VBISAM)
	LIBCOB_LIBS="$LIBCOB_LIBS -lvbisam"
	WITH_ISAM_DEFINED=yes
    fi ], [])
fi

AM_CONDITIONAL(WITH_ISAM, test x$WITH_ISAM_DEFINED = xyes)

if test "$with_cisam" != "yes" -a "$with_disam" != "yes" -a "$with_vbisam" != "yes"
then
	COB_SPLITKEY_FLAGS="no"
else
	COB_SPLITKEY_FLAGS="yes"
fi

LIBDB=""

if test "$with_cisam" != "yes" -a "$with_disam" != "yes" -a "$with_vbisam" != "yes"
then
 AC_ARG_WITH(index-extfh,
  [  --with-index-extfh      (OpenCOBOL) Use external ISAM file handler],
  [ if test "$with_index_extfh" = "yes"
    then
	AC_DEFINE(WITH_INDEX_EXTFH)
    fi ], [])
fi

if test "$with_cisam" != "yes" -a "$with_disam" != "yes" -a "$with_vbisam" != "yes" -a "$with_index_extfh" != "yes"
then
  AC_ARG_WITH(db1,
   [  --with-db1              (OpenCOBOL) use Berkeley DB 1.85 (libdb-1.85)],
   [ LIBDB="1"
     AC_DEFINE(WITH_DB, 1)
     with_db=yes ],
	[ AC_ARG_WITH(db,
		[  --with-db               (OpenCOBOL) use Berkeley DB 3.0 or later (libdb)(default)],
		[ LIBDB="3"
		  AC_DEFINE(WITH_DB, 3) 
        ],
		[ LIBDB="3"
		  AC_DEFINE(WITH_DB, 3)
        with_db=yes ]) ])
fi

if test "$with_cisam" != "yes" -a "$with_disam" != "yes" -a "$with_vbisam" != "yes" -a "$with_index_extfh" != "yes" -a "$with_db" != "yes"
then
	COB_HAS_ISAM="no"
else
	COB_HAS_ISAM="yes"
fi

#AC_ARG_WITH(lfs64,
#  [  --with-lfs64            (OpenCOBOL) use large file system for file I/O (default)],
#  [ if test "$with_lfs64" = "yes"
#    then
#	AC_DEFINE(WITH_LFS64)
#    fi ],
#  [ AC_DEFINE(WITH_LFS64) ])

DEFINE_DL="no"
COB_TRY_DL="yes"

AC_ARG_WITH(dl,
  [  --with-dl               (OpenCOBOL) use system dynamic loader (default)],
  [case $with_dl in  
    yes)	COB_TRY_DL="yes"
		;;
    no)		COB_TRY_DL="no"
		;;
    *)		AC_MSG_ERROR(["--with/without-dl can not have a value"])
		;;
  esac])

GEN_DEBUG_OPT="no"
AC_ARG_ENABLE(debug,
  [  --enable-debug          (OpenCOBOL) enable -g C compiler debug option],
  [if test "$enable_debug" = "yes"
   then
	GEN_DEBUG_OPT="yes"
   fi])

AC_ARG_ENABLE(experimental,
  [  --enable-experimental   (OpenCOBOL) enable experimental code (Developers only!)],
  [if test "$enable_experimental" = "yes"
   then
	AC_DEFINE(COB_EXPERIMENTAL)
   fi])

AC_ARG_ENABLE(param-check,
  [  --enable-param-check    (OpenCOBOL) enable CALL parameter checking],
  [if test "$enable_param_check" = "yes"
   then
	AC_DEFINE(COB_PARAM_CHECK)
   fi])

AC_ARG_ENABLE(utf8,
  [  --enable-utf8           (OpenCOBOL) enable UTF-8 support.],
  [if test "$enable_utf8" = "yes"
   then
	AC_DEFINE(I18N_UTF8)
	COB_M4_FLAGS=-DM4.I18N_UTF8
   fi])
AM_CONDITIONAL(I18N_UTF8, test x$COB_M4_FLAGS = x-DM4.I18N_UTF8)

PATCH_LEVEL=0
AC_ARG_WITH(patch-level, [  --with-patch-level      (OpenCOBOL) define a patch level (default 0)],
[case $with_patch_level in  
    yes)	AC_MSG_ERROR(["You must give --with-patch-level an argument."])
		;;
    no)		AC_MSG_ERROR(["--without-patch-level not supported."])
		;;
    [[0-9]]*)	PATCH_LEVEL=$with_patch_level
		;;
    *)		AC_MSG_ERROR(["You must use a numeric patch level"])
		;;
esac])
AC_DEFINE_UNQUOTED(PATCH_LEVEL, $PATCH_LEVEL)

WITH_VARSEQ=0
AC_ARG_WITH(varseq, [  --with-varseq           (OpenCOBOL) define variable sequential format (default 0)],
[case $with_varseq in  
    yes)	AC_MSG_ERROR(["You must give --with-varseq an argument."])
		;;
    no)		AC_MSG_ERROR(["--without-varseq not supported."])
		;;
    [[0-3]])	WITH_VARSEQ=$with_varseq
		;;
    *)		AC_MSG_ERROR(["Invalid --with-varseq argument"])
		;;
esac])
AC_DEFINE_UNQUOTED(WITH_VARSEQ, $WITH_VARSEQ)

# Checks for programs.
# Stop the stupid tests for C++ and Fortran
AC_PROVIDE([AC_PROG_F77])
AC_PROVIDE([AC_PROG_CXX])
with_tags=
AC_PROG_LN_S
AC_PROG_INSTALL
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdint.h sys/types.h signal.h fcntl.h malloc.h locale.h \
	stddef.h wchar.h dlfcn.h sys/time.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_STRUCT_TM
# AX_C___ATTRIBUTE__
AC_C_INLINE
if test "$ac_cv_c_inline" != no 
then
	AC_DEFINE(COB_HAS_INLINE, 1)
fi


# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset setlocale fcntl strerror \
		strcasecmp strchr strrchr strdup strstr strtol gettimeofday])

# Check for timezone
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]], [[timezone = 3600;]])],
	[AC_DEFINE(HAVE_TIMEZONE, 1)],
	[])

# Checks for internationalization stuff
AM_ICONV
AM_LANGINFO_CODESET

# Checks for getopt_long.
# Cygwin has getopt_long_only prototype in kpathsea/getopt.h
# FreeBSD has gnugetopt library - needs checking

AC_CHECK_FUNC([getopt_long_only],
  [AC_CHECK_HEADERS([kpathsea/getopt.h getopt.h], [], [])],
  [AC_LIBOBJ(getopt)
   AC_LIBOBJ(getopt1)
   AC_LIBSOURCE(getopt.h)])

# Checks for gmp.
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp.h is required))
AC_CHECK_LIB([gmp], [__gmp_randinit], , AC_MSG_ERROR(GMP 3 or later is required))
LIBCOB_LIBS="$LIBCOB_LIBS -lgmp"

# Solaris has nanosleep in other libraries
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
  [[nanosleep (NULL, NULL);]])],
  [],
  [AC_CHECK_LIB([rt], [nanosleep])
   if test "x$ac_cv_lib_rt_nanosleep" = "xyes"
   then
     LIBCOB_LIBS="$LIBCOB_LIBS -lrt"
   else
     AC_CHECK_LIB([posix4], [nanosleep])
     if test "x$ac_cv_lib_posix4_nanosleep" = "xyes"
     then
       LIBCOB_LIBS="$LIBCOB_LIBS -lposix4"
     fi
   fi])

# Checks for gettext.
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION(0.13)
if test "x$LTLIBINTL" != "x"
then
  COBC_LIBS="$COBC_LIBS $LTLIBINTL"
  LIBCOB_LIBS="$LIBCOB_LIBS $LTLIBINTL"
fi

# Checks for ncurses/pdcurses/curses.
USE_NCURSES="no"
AC_CHECK_LIB([ncurses], [initscr])
if test "x$ac_cv_lib_ncurses_initscr" = "xyes"
then
  LIBCOB_LIBS="$LIBCOB_LIBS -lncurses"
  AC_CHECK_HEADERS([ncurses.h], [USE_NCURSES="yes"],
    [AC_CHECK_HEADERS([ncurses/ncurses.h], [USE_NCURSES="ncurses"], [], [])], [])
else
  AC_CHECK_LIB([pdcurses], [initscr])
  if test "x$ac_cv_lib_pdcurses_initscr" = "xyes"
  then
    LIBCOB_LIBS="$LIBCOB_LIBS -lpdcurses"
    AC_CHECK_HEADERS([pdcurses.h], [USE_NCURSES="pdcurses"], [], [])
  else
    AC_CHECK_LIB([curses], [initscr])
    if test "x$ac_cv_lib_curses_initscr" = "xyes"
    then
      LIBCOB_LIBS="$LIBCOB_LIBS -lcurses"
      AC_CHECK_HEADERS([curses.h], [USE_NCURSES="curses"], [], [])
    fi
  fi
fi

if test "$USE_NCURSES" != "no"
then
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #ifdef HAVE_NCURSES_H
  #include <ncurses.h>
  #elif defined (HAVE_NCURSES_NCURSES_H)
  #include <ncurses/ncurses.h>
  #elif defined (HAVE_PDCURSES_H)
  #include <pdcurses.h>
  #elif defined (HAVE_CURSES_H)
  #include <curses.h>
  #endif]], [[
  color_set (0, NULL);]])],
  [AC_DEFINE(HAVE_COLOR_SET)],
  [],
  [])
fi

# Checks for db.
USE_DB="no"
HAS_DB41="no"
if test "$LIBDB" = "1"
then
       AC_CHECK_HEADERS([db1/db.h],
         [AC_DEFINE(HAVE_DB1_DB_H)],
         [AC_MSG_ERROR([db1/db.h is required])])
       AC_CHECK_LIB([db1], [dbopen],
         [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb1"],
         [AC_MSG_ERROR(libdb1 is required)])
elif test "$LIBDB" = "3"
then
	AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
		#include <db.h>]], [[
		printf("checking for Berkeley DB db.h version >= 4.1 ... ");
		#if (defined(DB_VERSION_MAJOR) && defined(DB_VERSION_MINOR))
		#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 0))
		printf("yes (%d.%d)\n", DB_VERSION_MAJOR, DB_VERSION_MINOR);
		return 0;
		#else
		printf("no (%d.%d)\n", DB_VERSION_MAJOR, DB_VERSION_MINOR);
		return 1;
		#endif
		#else
		printf("no (version numbers not found)\n");
		return 1;
		#endif]])],
		[HAS_DB41="yes"],
		[AC_MSG_ERROR([Berkeley DB db.h is missing or has incompatible version])],
		[])
	if test "$HAS_DB41" = "yes"
	then
		AC_DEFINE(USE_DB41)
		MYOLDLIBS="$LIBS"
		export MYOCLIBS
		for MYOCLIBS in db db4 db-4.5 db-4.4 db-4.3 db-4.2 db-4.1
		do
			LIBS="$MYOLDLIBS -l$MYOCLIBS"
			AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
			#include <stdlib.h>
			#include <db.h>]], [[
			int major, minor, patch;
			printf("checking for BDB 4.1 compatibility in %s ... ", getenv("MYOCLIBS"));
			db_version (&major, &minor, &patch);
			if (major == DB_VERSION_MAJOR && minor == DB_VERSION_MINOR) {
				printf("yes\n");
				return 0;
			}
			printf("no\n");
			return 1;]])],
			[USE_DB="yes"],
			[],
			[])
			if test "$USE_DB" != "no"
			then
				LIBCOB_LIBS="$LIBCOB_LIBS -l$MYOCLIBS"
				break
			fi
		done
		LIBS="$MYOLDLIBS"
		if test "$USE_DB" != "yes"
		then
			AC_MSG_ERROR(Include file db.h implies version >=4.1 but no library found)
		fi
	else
		AC_CHECK_HEADERS([db.h db_185.h db3/db_185.h db4/db_185.h db4.1/db_185.h db4.2/db_185.h db4.3/db_185.h db4.4/db_185.h db4.5/db_185.h])
		AC_CHECK_LIB([db-4.5], [__db_open],
		[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.5"],
		  [AC_CHECK_LIB([db-4.5], [dbopen],
		    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.5"], [])])
		if test "$USE_DB" != "yes"
		then
		AC_CHECK_LIB([db-4.4], [__db_open],
		[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.4"],
		  [AC_CHECK_LIB([db-4.4], [dbopen],
		    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.4"], [])])
		fi
		if test "$USE_DB" != "yes"
		then
		AC_CHECK_LIB([db-4.3], [__db_open],
		[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.3"],
		  [AC_CHECK_LIB([db-4.3], [dbopen],
		    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.3"], [])])
		fi
		if test "$USE_DB" != "yes"
		then
		AC_CHECK_LIB([db-4.2], [__db_open],
		[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.2"],
		  [AC_CHECK_LIB([db-4.2], [dbopen],
		    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.2"], [])])
		fi
		if test "$USE_DB" != "yes"
		then
		AC_CHECK_LIB([db-4.1], [__db_open],
		[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.1"],
		  [AC_CHECK_LIB([db-4.1], [dbopen],
		    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.1"], [])])
		fi
		if test "$USE_DB" != "yes"
		then
#			if test "$HAS_DB41" != "no"
#			then
#				AC_MSG_ERROR(Include file db.h implies version >=4.1 but no library found)
#			fi
		AC_CHECK_LIB([db-4], [__db_open],
		[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4"],
		  [AC_CHECK_LIB([db-4], [dbopen],
		    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4"], [])])
		fi
		
		if test "$USE_DB" != "yes"
		then
		AC_CHECK_LIB([db4], [__db_open],
		[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb4"],
		  [AC_CHECK_LIB([db4], [dbopen],
		    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb4"], [])])
		fi
		
		if test "$USE_DB" != "yes"
		then
		AC_CHECK_LIB([db], [__db_open],
		[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb"],
		  [AC_CHECK_LIB([db], [dbopen],
		    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb"],
			[AC_MSG_ERROR(libdb is required)])])
		fi
	fi
fi

# Checks for dl/ltdl.
AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
  #ifdef _WIN32
   return 0;
  #else
   return 1;
  #endif]])],
  [DEFINE_DL="yes"],
  [],
  [])

if test "$DEFINE_DL" = "no" -a "$COB_TRY_DL" = "yes"
then
  if test "x$ac_cv_header_dlfcn_h" = "xyes"
  then
    AC_CHECK_LIB([c], [dlopen], [DEFINE_DL="yes"], [])
    if test "$DEFINE_DL" = "no"
    then
      AC_CHECK_LIB([dl], [dlopen], [DEFINE_DL="yes"], [])
      if test "$DEFINE_DL" = "yes"
      then
	AC_DEFINE(USE_LIBDL)
	LIBCOB_LIBS="$LIBCOB_LIBS -ldl"
      fi
    else
	AC_DEFINE(USE_LIBDL)
    fi
  fi
fi
if test "$DEFINE_DL" = "no"
then
  AC_CHECK_HEADERS([ltdl.h], , AC_MSG_ERROR(ltdl.h is required))
  AC_CHECK_LIB([ltdl], [lt_dlopen], , AC_MSG_ERROR(libltdl is required))
  LIBCOB_LIBS="$LIBCOB_LIBS -lltdl"
fi

# Checks for workable strftime
AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
	#include <time.h>]], [[
	time_t	ticks;
	int	ret;
	char	buff[16];
	ticks = time(NULL);
	ret = strftime(buff, 15, "%z", localtime(&ticks));
	if (ret != 5) {
		return 1;
	}
	if (buff[0] != '+' && buff[0] != '-') {
		return 1;
	}
	return 0;]])],
	[AC_DEFINE(COB_STRFTIME)],
	[],
	[])

# Checks for size of long
AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	if (sizeof(long int) == sizeof(long long))
		return 0;
	return 1;
	]])],
	[AC_DEFINE(COB_LI_IS_LL)],
	[],
	[])

# OpenCOBOL Configuration

if test "x$prefix" = "xNONE"
then
  if test "x$includedir" = "x\${prefix}/include"
  then
    COB_CFLAGS="-I${ac_default_prefix}/include"
  else
    COB_CFLAGS="-I${includedir}"
  fi
else
  if test "x$includedir" = "x\${prefix}/include"
  then
    if test "x$prefix" = "x/usr"
    then
      COB_CFLAGS=""
    else
      COB_CFLAGS="-I${prefix}/include"
    fi
  else
    COB_CFLAGS="-I${includedir}"
  fi
fi

if test "x$CPPFLAGS" != "x"
then
  COB_CFLAGS="$COB_CFLAGS $CPPFLAGS"
fi

COB_LIBS="-L$libdir -lcob ${LIBCOB_LIBS}"
COB_LDFLAGS=""
if test "x$LDFLAGS" != "x"
then
  COB_LIBS="$LDFLAGS $COB_LIBS"
  COB_LDFLAGS="$LDFLAGS"
fi

COB_CONFIG_DIR="$datadir/$PACKAGE_TARNAME/config"
COB_COPY_DIR="$datadir/$PACKAGE_TARNAME/copy"
COB_LIBRARY_PATH="$libdir/$PACKAGE_TARNAME"
COB_MODULE_EXT=`echo "$shlibext" | sed -e 's/dll\.a/dll/'`
LIBS="$save_libs"
COB_EXPORT_DYN="`eval echo $export_dynamic_flag_spec`"
COB_PIC_FLAGS=`echo "$lt_prog_compiler_pic" | sed -e 's/^ //'`
COB_SHARED_OPT="-shared"
COB_EXEEXT="$EXEEXT"
COB_EXTRA_FLAGS=""

if test "$GEN_DEBUG_OPT" = "no"
then
        CFLAGS=`echo "$CFLAGS" | sed -e 's/^-g//' -e 's/-g$//' -e 's/-g / /'`
fi

# Include CFLAGS in COB_CFLAGS without optimization/debug options

cob_temp_flags=`echo "$CFLAGS" | sed -e 's/^@<:@+-@:>@O@<:@0-9s@:>@//' -e 's/@<:@+-@:>@O@<:@0-9s@:>@$//' -e 's/@<:@+-@:>@O@<:@0-9s@:>@ / /'`
cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/^-O//' -e 's/-O$//' -e 's/-O / /' -e 's/^-g//' -e 's/-g$//' -e 's/-g / /'`

COB_CFLAGS="$COB_CFLAGS $cob_temp_flags"

if test "`basename $COB_CC`" = "gcc"
then
	cob_temp_flags="`${COB_CC} ${srcdir}/cpucheck.c -o cpucheck 1>/dev/null 2>&1 && ./cpucheck 2>/dev/null`"
	rm -f cpucheck${EXEEXT}
	if test "x$cob_temp_flags" != "x"
	then
		CFLAGS="$CFLAGS $cob_temp_flags -finline-functions"
		COB_EXTRA_FLAGS="$cob_temp_flags"
	else
		CFLAGS="$CFLAGS -finline-functions"
	fi
fi

# Check gcc 4 option
MYOLDCFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -Wno-pointer-sign"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
	[AC_DEFINE(HAVE_PSIGN_OPT, 1)],
	[])
CFLAGS="$MYOLDCFLAGS"

# Check if aligned attribute works
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],
	[[char testchar[4] __attribute__((aligned));]])],
	[AC_DEFINE(HAVE_ATTRIBUTE_ALIGNED, 1)],
	[])

# Special stuff

AH_VERBATIM([_XOPEN_SOURCE_EXTENDED],
[/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
# undef _XOPEN_SOURCE_EXTENDED
#endif])dnl

COB_FIX_LIB="$COB_EXPORT_DYN"
case $host_os in
  mingw*)
	if test "$prefix" = "NONE"
	then
	COB_CONFIG_DIR="`cd /usr && pwd -W`/local/share/$PACKAGE_TARNAME/config"
	COB_COPY_DIR="`cd /usr && pwd -W`/local/share/$PACKAGE_TARNAME/copy"
	fi
	COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import"
	COB_FIX_LIB="$COB_EXPORT_DYN"
	;;
  cygwin*)
	COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import"
	COB_FIX_LIB="$COB_EXPORT_DYN"
	;;
  darwin* | rhapsody*)
	COB_SHARED_OPT="-bundle -flat_namespace -undefined suppress"
	;;
  hpux*)
	if test "`basename $CC`" != "gcc"
	then
		COB_SHARED_OPT="-b"
		AC_DEFINE([_XOPEN_SOURCE_EXTENDED],1)
		rm -f hptest*
		echo 'int main() { return 0; }' > hptest.c
		${CC} ${CFLAGS} +Olit=all -o hptest hptest.c > hptest.out 2>&1
		if test $? -ne 0 -o -s hptest.out
		then
			CFLAGS="$CFLAGS +ESlit"
			COB_CFLAGS="$COB_CFLAGS +ESlit -w"
		else
			CFLAGS="$CFLAGS +Olit=all"
			COB_CFLAGS="$COB_CFLAGS +Olit=all -w"
		fi
		rm -f hptest*
	fi
	;;
  aix*)
	COB_EXPORT_DYN="-Wl,-bexpfull"
	if test "`basename $CC`" != "gcc"
	then
		CFLAGS="$CFLAGS -O2 -Q -qro -qroconst"
		COB_SHARED_OPT="-G"
		COB_FIX_LIB=""
		# COB_FIX_LIB="-Wc,-G $COB_EXPORT_DYN"
		# COB_CFLAGS="$COB_CFLAGS -qchars=signed"
	else
		COB_FIX_LIB="$COB_EXPORT_DYN"
	fi
	;;
  solaris*)
	if test "`basename $CC`" != "gcc"
	then
		COB_SHARED_OPT="-G"
	fi
	;;
esac

if test "`basename $CC`" = "gcc"
then
	CFLAGS="$CFLAGS -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k"
fi

if test "x$lt_cv_dlopen_self" != "xyes"
then
	AC_DEFINE(COB_NO_SELFOPEN,1)
fi

# Generate the output

AC_DEFINE_UNQUOTED(COB_EXPORT_DYN, "$COB_EXPORT_DYN")
AC_DEFINE_UNQUOTED(COB_PIC_FLAGS, "$COB_PIC_FLAGS")
AC_DEFINE_UNQUOTED(COB_SHARED_OPT, "$COB_SHARED_OPT")
AC_DEFINE_UNQUOTED(COB_EXEEXT, "$COB_EXEEXT")
AC_DEFINE_UNQUOTED(COB_EXTRA_FLAGS, "$COB_EXTRA_FLAGS")
if test "x$striplib" != "x"
then
  AC_DEFINE_UNQUOTED(COB_STRIP_CMD, "$striplib")
# AC_SUBST(COB_STRIP_CMD)
fi
AC_SUBST(COB_CC)
AC_SUBST(COB_CFLAGS)
AC_SUBST(COB_LDFLAGS)
AC_SUBST(COB_LIBS)
AC_SUBST(COB_CONFIG_DIR)
AC_SUBST(COB_COPY_DIR)
AC_SUBST(COB_LIBRARY_PATH)
AC_SUBST(COB_MODULE_EXT)
AC_SUBST(COBC_LIBS)
AC_SUBST(LIBCOB_CFLAGS)
AC_SUBST(LIBCOB_LIBS)
AC_SUBST(COB_EXPORT_DYN)
AC_SUBST(COB_PIC_FLAGS)
AC_SUBST(COB_SHARED_OPT)
COB_BIGENDIAN="$ac_cv_c_bigendian"
AC_SUBST(COB_BIGENDIAN)
AC_SUBST(COB_FIX_LIB)
AC_SUBST(COB_HAS_INLINE)
AC_SUBST(COB_HAS_ISAM)
AC_SUBST(COB_SPLITKEY_FLAGS)
AC_SUBST(COB_M4_FLAGS)

AC_CONFIG_FILES([bin/cob-config
		Makefile lib/Makefile libcob/Makefile
		cobc/Makefile bin/Makefile po/Makefile.in texi/Makefile config/Makefile
		copy/Makefile tests/atlocal tests/Makefile tests/cobol85/Makefile])

AC_CONFIG_COMMANDS([chmod1], [chmod +x bin/cob-config])
AC_CONFIG_COMMANDS([chmod2], [chmod +x tests/atconfig])
AC_CONFIG_COMMANDS([chmod3], [chmod +x tests/atlocal])

AC_OUTPUT

echo ""
echo "OpenCOBOL Configuration:"
echo ""
echo "  CC                   ${CC}"
echo "  COB_CC               ${COB_CC}"
echo "  CFLAGS               ${CFLAGS}"
echo "  COB_CFLAGS           ${COB_CFLAGS}"
echo "  COB_EXTRA_FLAGS      ${COB_EXTRA_FLAGS}"
echo "  LDFLAGS              ${LDFLAGS}"
echo "  COB_LDFLAGS          ${COB_LDFLAGS}"
if test "x$COBC_LIBS" != "x"
then
  echo "  COBC_LIBS            ${COBC_LIBS}"
fi
echo "  COB_LIBS             ${COB_LIBS}"
echo "  COB_CONFIG_DIR       ${COB_CONFIG_DIR}"
echo "  COB_COPY_DIR         ${COB_COPY_DIR}"
echo "  COB_LIBRARY_PATH     ${COB_LIBRARY_PATH}"
echo "  COB_MODULE_EXT       ${COB_MODULE_EXT}"
echo "  COB_SHARED_OPT       ${COB_SHARED_OPT}"
echo "  COB_PIC_FLAGS        ${COB_PIC_FLAGS}"
echo "  COB_EXPORT_DYN       ${COB_EXPORT_DYN}"
if test "x$striplib" != "x"
then
  echo "  COB_STRIP_CMD        ${striplib}"
fi
if test "${DEFINE_DL}" = "yes" 
then
  echo "  Dynamic loading      System"
else
  echo "  Dynamic loading      Libtool"
fi
echo ""
echo "  Use gettext for international messages:      ${USE_NLS}"
if test "$with_seqra_extfh" = "yes"
then
  echo "  Use external SEQ/RAN file handler            yes"
fi
if test "$with_index_extfh" = "yes"
then
  echo "  Use external ISAM file handler               yes"
elif test "$with_cisam" = "yes"
then
  echo "  Use CISAM for ISAM I/O                       yes"
elif test "$with_disam" = "yes"
then
  echo "  Use DISAM for ISAM I/O                       yes"
elif test "$with_vbisam" = "yes"
then
  echo "  Use VBISAM for ISAM I/O                      yes"
elif test "$with_db" = "yes"
then
  echo "  Use Berkeley DB for ISAM I/O:                yes"
  echo "  Use enhanced Berkeley DB (>= 4.1)            ${HAS_DB41}"
else
  echo "  ISAM I/O (no handler configured):            NO"
fi
echo "  Use fcntl for file locking:                  ${ac_cv_func_fcntl}"
echo "  Use ncurses/pdcurses/curses for screen I/O:  ${USE_NCURSES}"
echo ""
