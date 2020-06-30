#
# Makefile opencobol/libcob
# Process this file with automake to produce Makefile.in
#
# Copyright (C) 2003,2004,2005,2006,2007 Keisuke Nishida
# Copyright (C) 2008-2011 Roger While
#
# This file is part of OpenCOBOL.
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

lib_LTLIBRARIES = libcob.la
libcob_la_SOURCES = common.c call.c strings.c move.c numeric.c \
	intrinsic.c fileio.c termio.c screenio.c

libcob_la_CFLAGS = $(AM_CFLAGS) $(LIBCOB_CFLAGS) -I$(top_srcdir)
libcob_la_LIBADD = $(LIBCOB_LIBS)
libcob_la_LDFLAGS = -version-info 1:0:0 -no-undefined
AM_LDFLAGS = $(COB_FIX_LIB)

EXTRA_DIST = coblocal.h
pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = byteswap.h common.h move.h numeric.h exception.def \
	termio.h fileio.h screenio.h strings.h call.h intrinsic.h \
	codegen.h system.def
