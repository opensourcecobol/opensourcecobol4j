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

	int		iloop=0, ireadcount=0, irecount=0, 
			iresult=0, ifilehandle,
			key0, key1;
	int		i;
	struct keydesc	skeydesc01, skeydesc02;
	char		cfilename[] = "detest00";
	char		line0[21]="_aaaaaaaaaaaaaaaaaaa",
			line1[38]="_ddddddddddddddddddddddddddddddddddd ";
	unsigned char	crecord [MAXLEN+128];

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
		fprintf (stderr, "Usage: %s : { number of records }\n", ppcargv [0]);
		exit (1);
	} else {
		iloop = atoi(ppcargv [1]);
	}

	iserase (cfilename);

	ifilehandle = isbuild (cfilename, RECLEN, &skeydesc01, ISINOUT + ISFIXLEN + ISEXCLLOCK);
	if (ifilehandle < 0) {
		printf ("Error opening database: %d\n", iserrno);
		exit (2);
	}

	iresult = isaddindex (ifilehandle, &skeydesc02);
	if (iresult) {
		printf ("Error adding index 2 to database: %d\n", iserrno);   
		exit (3);
	}

	for (i = 0; i < iloop; i++)  {
		key0=i; 
		key1=1+(int) (9999.0*rand()/(RAND_MAX+1.0)); 
		sprintf((char *)crecord, "%05i%20s%03i%37s", key0, line0, key1, line1);
		if (iswrite (ifilehandle, (char *)crecord))  {
			printf ("Error %d writing row %d to file\n", iserrno, iloop);
			i = iloop;
		} else {
			irecount++;
		}
	}

	sprintf((char *)crecord, "%03i%20s%03i%37s", 0, line0, 0, line1);
	iresult = isread (ifilehandle, (char *)crecord, ISFIRST);
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
	fprintf (stdout, "Records created : %8d\n", irecount);
	fprintf (stdout, "Records read    : %8d\n", ireadcount);
	
	return 0;
}
