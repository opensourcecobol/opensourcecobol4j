/*
 * Copyright (C) 2007 Roger While
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#include	<stdio.h>
#include	"vbisam.h"

int
main (int iargc, char **ppcargv)
{
	int iloop;

	if (iargc == 1) {
		printf ("Usage:\n\t%s [FILENAME...]\n", ppcargv[0]);
		return 1;
	}

	for (iloop = 1; iloop < iargc; iloop++) {
		ischeck (ppcargv[iloop]);
	}
	return 0;
}
