# Copyright (C) 2003,2004,2005,2006,2007 Keisuke Nishida
# Copyright (C) 2008-2011 Roger While
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
