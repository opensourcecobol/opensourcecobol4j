#
# Makefile opencobol/cobj
# Process this file with automake to produce Makefile.in
#
# Copyright (C) 2003,2004,2005,2006,2007 Keisuke Nishida
# Copyright (C) 2008-2011 Roger While
# Copyright (C) 2020 TOKYO SYSTEM HOUSE Co., Ltd.
#
# This file is part of OpenCOBOL.
#
# The OpenCOBOL compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# OpenCOBOL is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with OpenCOBOL.  If not, see <http://www.gnu.org/licenses/>.

bin_PROGRAMS = cobj
cobj_SOURCES = cobj.c cobj.h config.c tree.c tree.h reserved.c error.c \
	parser.c parser.h scanner.c field.c typeck.c codegen.c \
	ppparse.c ppparse.h pplex.c config.def flag.def warning.def

cobj_CFLAGS = -I$(top_srcdir)
cobj_LDADD = $(COBJ_LIBS) $(top_builddir)/lib/libsupport.a

BUILT_SOURCES = parser.c parser.h scanner.c ppparse.c ppparse.h pplex.c
EXTRA_DIST = ppparse.y pplex.l parser.y scanner.l pplex.l.m4 scanner.l.m4
CLEANFILES = parser.output

%.c: %.l
	flex --nounistd -s -o$@ $<

%.c: %.y
	bison -o $@ $<

%.l: %.l.m4
	m4 $(COB_M4_FLAGS) $< >$@

pplex.l: pplex.l.m4
scanner.l: scanner.l.m4

