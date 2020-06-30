/*
* Copyright:    (C) 2005 David Essex
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
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<fcntl.h>
#include	<unistd.h>
#include	<stdlib.h>
#include	<string.h>
#include	<errno.h>
#include	<limits.h>
#include        <time.h>

#include        <vbisam.h>

#define MAXLEN	256
#define RECLEN	63

int main (int iargc, char **ppcargv) {

	int		ireadcount=0, iresult=0, 
			ifilehandle, key0, key1;
	struct keydesc	skeydesc01, skeydesc02;
	char		line0[21]="_aaaaaaaaaaaaaaaaaaa",
			line1[38]="_ddddddddddddddddddddddddddddddddddd ";
	unsigned char	crecord [MAXLEN];

	memset (&skeydesc01, 0, sizeof (skeydesc01));
	skeydesc01.k_flags = ISNODUPS;
	skeydesc01.k_nparts = 1;
	skeydesc01.k_start = 0;
	skeydesc01.k_leng = 5;
	skeydesc01.k_type = CHARTYPE;

	memset (&skeydesc02, 0, sizeof (skeydesc02));
	skeydesc02.k_flags = ISDUPS;
	skeydesc02.k_nparts = 1;
	skeydesc02.k_start = 23;
	skeydesc02.k_leng = 3;
	skeydesc02.k_type = CHARTYPE;

	if (iargc != 2)  {
		fprintf (stderr, "Usage: %s : { input file name prefix }\n", ppcargv [0]);
		exit (1);
	} else {
		ifilehandle = isopen (ppcargv[1], ISINPUT+ISFIXLEN+ISAUTOLOCK);
		if (ifilehandle < 0) {
			printf ("Error (%d) opening file %s\n", iserrno, ppcargv[1]);
			exit (2);
		}
	}

	key0 = 0; 
	key1 = 800; /* Start position of secondary key */
	sprintf((char *)crecord, "%03i%20s%03i%37s", key0, line0, key1, line1);

	iresult = isstart (ifilehandle, &skeydesc02, RECLEN, (char *)crecord, ISFIRST);
	iresult = isread (ifilehandle, (char *)crecord, ISGTEQ);
	while (iresult == 0) {
		ireadcount++;
		crecord [RECLEN + 1] = '\0';
		fprintf (stdout, "%c%c%c %c%c%c\n",  
			crecord[23], crecord[24], crecord[25], 
			crecord[0], crecord[1], crecord[2]);
		iresult = isread (ifilehandle, (char *)crecord, ISNEXT);
	}

	if (iserrno != EENDFILE) {
		fprintf (stderr, "Error : iserrno=%d\n", iserrno);
	}

	fprintf (stdout, "Audit summary:\n");
	fprintf (stdout, "Records read: %8d\n", ireadcount);

	return 0;
}
