#                                                       -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2005-2007 Roger While
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

AC_PREREQ(2.59)

AC_INIT([VBISAM],[2.0],[vbisam-list@lists.sourceforge.net],[vbisam])
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
AH_TEMPLATE([ISAMMODE], [ISAM mode 0=C-ISAM compatible, 1=Extended])
AH_TEMPLATE([WITH_LFS64], [Compile with large file system I/O])
AH_TEMPLATE([VBDEBUG], [Turn on debugging mode])
AH_TEMPLATE([__USE_STRING_INLINES], [Compiler optimization])

AC_DEFINE(__USE_STRING_INLINES)

AC_ARG_WITH(lfs64,
  [  --with-lfs64            (VBISAM) use large file system for file I/O (default)],
  [ if test "$with_lfs64" = "yes"
    then
	AC_DEFINE(WITH_LFS64)
    fi ],
  [ AC_DEFINE(WITH_LFS64) ])

USE_DEBUG="no"
AC_ARG_WITH(debug,
  [  --with-debug            (VBISAM) Turn on debugging mode],
  [ USE_DEBUG="yes"
    AC_DEFINE(VBDEBUG) ])

USE_MODE1="yes"
AC_ARG_WITH(compatcisam,
  [  --with-compatcisam      (VBISAM) C-ISAM compatible mode],
  [ USE_MODE1="no" ])

if test "${USE_MODE1}" = "no"
then
	AC_DEFINE(ISAMMODE, 0)
else
	AC_DEFINE(ISAMMODE, 1)
	AC_DEFINE(WITH_LFS64)
fi

# Checks for programs.
AC_PROG_CC
AC_PROVIDE([AC_PROG_F77])
AC_PROVIDE([AC_PROG_CXX])
with_tags=
AC_PROG_LN_S
AC_PROG_INSTALL
# AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdint.h sys/types.h fcntl.h malloc.h stddef.h wchar.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_TYPE_LONG_LONG
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset fcntl \
		strcasecmp strchr strrchr strdup strstr strtol])

if test "`basename $CC`" = "gcc"
then
	vb_temp_flags="`${CC} ${srcdir}/cpucheck.c -o cpucheck 1>/dev/null 2>&1 && ./cpucheck 2>/dev/null`"
	rm -f cpucheck${EXEEXT}
	if test "x$vb_temp_flags" != "x"
	then
		CFLAGS="$CFLAGS $vb_temp_flags -finline-functions"
	else
		CFLAGS="$CFLAGS -finline-functions"
	fi
fi

AH_VERBATIM([_XOPEN_SOURCE_EXTENDED],
[/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
# undef _XOPEN_SOURCE_EXTENDED
#endif])dnl

case $host_os in
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
		else
			CFLAGS="$CFLAGS +Olit=all"
		fi
		rm -f hptest*
	fi
	;;
  aix*)
	if test "`basename $CC`" != "gcc"
	then
		CFLAGS="$CFLAGS -O2 -Q -qro -qroconst"
	fi
	;;
esac

if test "`basename $CC`" = "gcc"
then
	CFLAGS="$CFLAGS -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k"
fi

AC_CONFIG_FILES([Makefile libvbisam/Makefile bin/Makefile tests/atlocal tests/Makefile])
AC_OUTPUT

echo ""
echo "VBISAM Configuration:"
echo ""
echo "  CC                             ${CC}"
echo "  CFLAGS                         ${CFLAGS}"
if test "${USE_MODE1}" = "yes"
then
echo "  C-ISAM compatible mode         no"
else
echo "  C-ISAM compatible mode         yes"
fi
echo "  Debugging mode                 ${USE_DEBUG}"
echo ""
