/*
 * Copyright (C) 2004 Mikhail Verkhovski
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
	int	iresult;

	if (iargc != 2) {
		printf ("Usage: %s <LOGFILE>\n", ppcargv [0]);
		return 1;
	}
	iresult = islogopen (ppcargv[1]);
	if (iresult < 0) {
		fprintf (stdout, "Error opening log: %d\n", iserrno);
		return 1;
	}
	printf ("Recovering... Please wait\n");
	iresult = isrecover ();
	islogclose ();
	if (iresult) {
		printf ("Recovery failed with error %d\n", iserrno);
		return 1;
	}
	printf ("Recovery SUCCESSFUL!\n");
	return 0;
}
