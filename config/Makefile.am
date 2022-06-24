#
# Makefile opencobol/config
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

configdir = @COB_CONFIG_DIR@
config_DATA = default.conf cobol85.conf cobol2002.conf \
	mf.conf ibm.conf mvs.conf bs2000.conf

EXTRA_DIST = $(config_DATA) \
	default-en.conf \
	default-jp.conf \
	jp-compat.conf \
	boundary-limit.conf

#install-data-hook:
#$(configDATA_INSTALL) 'default-jp.conf' '$(DESTDIR)$(configdir)/default.conf'
