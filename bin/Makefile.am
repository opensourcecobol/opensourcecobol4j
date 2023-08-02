#
# Makefile opencobol/bin
# Process this file with automake to produce Makefile.in
#
# Copyright (C) 2004-2011 Roger While
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

bin_SCRIPTS=cob-config
bin_PROGRAMS = cobjrun
cobcrun_SOURCES = cobjrun.c

AM_LDFLAGS = $(COB_EXPORT_DYN)
cobcrun_CFLAGS = $(AM_CFLAGS) -I$(top_srcdir)