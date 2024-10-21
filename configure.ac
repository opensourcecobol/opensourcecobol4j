# Copyright (C) 2001-2009 Keisuke Nishida
# Copyright (C) 2007-2009 Roger While
# Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor
# Boston, MA 02110-1301 USA

AC_PREREQ([2.71])

AC_INIT([opensource COBOL 4J],[1.1.2],[ws-opensource-cobol-contact@osscons.jp],[opensource-cobol-4j-1.1.2])
AC_CONFIG_SRCDIR([libcobj.h])
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
AH_TEMPLATE([WITH_INDEX_EXTFH], [Compile with an external ISAM handler])
AH_TEMPLATE([WITH_SEQRA_EXTFH], [Compile with an external SEQ/RAN handler])
AH_TEMPLATE([COB_PARAM_CHECK], [Enable CALL parameter checking])
AH_TEMPLATE([I18N_UTF8], [Enable UTF-8 support])
AH_TEMPLATE([COB_EXPORT_DYN], [Compile/link option for exporting symbols])
AH_TEMPLATE([COB_PIC_FLAGS], [Compile/link option for PIC code])
AH_TEMPLATE([COB_SHARED_OPT], [Compile/link option for shared code])
AH_TEMPLATE([COB_STRIP_CMD], [Strip command])
AH_TEMPLATE([USE_LIBDL], [Use system dynamic loader])
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


#AC_ARG_WITH(lfs64,
#  [  --with-lfs64            (OpenCOBOL) use large file system for file I/O (default)],
#  [ if test "$with_lfs64" = "yes"
#    then
#	AC_DEFINE(WITH_LFS64)
#    fi ],
#  [ AC_DEFINE(WITH_LFS64) ])

GEN_DEBUG_OPT="no"
AC_ARG_ENABLE(debug,
  [  --enable-debug          (OpenCOBOL) enable -g C compiler debug option],
  [if test "$enable_debug" = "yes"
   then
	GEN_DEBUG_OPT="yes"
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
AC_PROG_RANLIB

# Checks for header files.
m4_warn([obsolete],
[The preprocessor macro `STDC_HEADERS' is obsolete.
  Except in unusual embedded environments, you can safely include all
  ISO C90 headers unconditionally.])dnl
# Autoupdate added the next two lines to ensure that your configure
# script's behavior did not change.  They are probably safe to remove.
AC_CHECK_INCLUDES_DEFAULT
AC_PROG_EGREP

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
		strcasecmp strchr strrchr strdup strstr strtol])

# Check for timezone
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]], [[timezone = 3600;]])],
	[AC_DEFINE(HAVE_TIMEZONE, 1)],
	[])

# Checks for internationalization stuff
AM_ICONV

# Checks for getopt_long.
# Cygwin has getopt_long_only prototype in kpathsea/getopt.h
# FreeBSD has gnugetopt library - needs checking

AC_CHECK_FUNC([getopt_long_only],
  [AC_CHECK_HEADERS([kpathsea/getopt.h getopt.h], [], [])],
  [AC_LIBOBJ(getopt)
   AC_LIBOBJ(getopt1)
   AC_LIBSOURCE(getopt.h)])

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
	CFLAGS="$CFLAGS -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-security"
fi

if test "x$lt_cv_dlopen_self" != "xyes"
then
	AC_DEFINE(COB_NO_SELFOPEN,1)
fi

COB_SPLITKEY_FLAGS="yes"

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

AC_CONFIG_FILES([Makefile lib/Makefile
		cobj/Makefile config/Makefile libcobj/Makefile
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
echo ""
echo "  Use gettext for international messages:      ${USE_NLS}"
if test "$with_seqra_extfh" = "yes"
then
  echo "  Use external SEQ/RAN file handler            yes"
fi
echo "  Use fcntl for file locking:                  ${ac_cv_func_fcntl}"
echo ""
