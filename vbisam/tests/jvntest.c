/*
 * Copyright (C) 2003 Trevor van Bremen
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
#include	<float.h>

#include	<vbisam.h>

#define	FILE_COUNT	1
#define	INDX_COUNT	2

static struct	keydesc gskey[10];
/*
	gskey [10] =
	{
		{ISNODUPS,	1,  {{0,  8, 0}},},
		{ISDUPS,	1, {{42, 14, 0}},},
	};
*/

int
main (int iargc, char **ppargv)
{
	char	cbuffer [1024],
		cname [32];
	int	ihandle [10],
		iloop,
		iloop1,
		iresult;
	FILE	*pshandle;

	memset ((char *)&gskey[0], 0, sizeof (gskey));
	gskey[0].k_flags = ISNODUPS;
	gskey[0].k_nparts = 1;
	gskey[0].k_part[0].kp_start = 0;
	gskey[0].k_part[0].kp_leng = 8;
	gskey[0].k_part[0].kp_type = 0;
	gskey[1].k_flags = ISDUPS;
	gskey[1].k_nparts = 1;
	gskey[1].k_part[0].kp_start = 42;
	gskey[1].k_part[0].kp_leng = 14;
	gskey[1].k_part[0].kp_type = 0;
	for (iloop = 0; iloop < INDX_COUNT; iloop++)
	{
		gskey [iloop].k_len = 0;
		for (iloop1 = 0; iloop1 < gskey [iloop].k_nparts; iloop1++)
			gskey [iloop].k_len += gskey [iloop].k_part [iloop1].kp_leng;
	}
	for (iloop = 0; iloop < FILE_COUNT; iloop++)
	{
		sprintf (cname, "File%d", iloop);
		iserase (cname);
		ihandle [iloop] = isbuild (cname, 170, &gskey [0], ISINOUT + ISEXCLLOCK + ISNOLOG);
		if (ihandle [iloop] < 0)
		{
			printf ("isbuild error %d for %s file\n", iserrno, cname);
			exit (1);
		}
	}
	for (iloop = 0; iloop < FILE_COUNT; iloop++)
	{
		for (iloop1 = 1; iloop1 < INDX_COUNT; iloop1++)
		{
			iresult = isaddindex (ihandle [iloop], &gskey [iloop1]);
			if (iresult)
			{
				printf ("isaddindex error %d on handle %d index %d\n", iserrno, iloop, iloop1);
				exit (1);
			}
		}
	}
/*
	isclose (ihandle [0]); ihandle [0] = isopen (cname, ISINOUT | ISEXCLLOCK | ISNOLOG);
*/
	pshandle = fopen ("TESTDATA", "r");
	if (pshandle == (FILE *) 0)
	{
		printf ("Error opening source file!\n");
		exit (1);
	}
	iloop1 = 0;
	while (fgets (cbuffer, 1024, pshandle) != NULL)
	{
		if (iloop1 == 0) {
			printf ("[%-8.8s] [%-14.14s]\n", cbuffer, cbuffer + 42);
		}
		iloop1++;
		cbuffer [170] = 0;
		for (iloop = 0; iloop < FILE_COUNT; iloop++)
			if (iswrite (ihandle [iloop], cbuffer))
			{
				printf ("Error %d writing row %d to file %d\n", iserrno, iloop1, iloop);
				exit (1);
			}
			if (iloop1 > atoi (ppargv [1])) {
				break;
			}
	}
	fclose (pshandle);
/*
	isclose (ihandle [0]); ihandle [0] = isopen (cname, ISINOUT | ISEXCLLOCK);
*/
	for (iloop = 0; iloop < 2; iloop++)
	{
		iresult = isread (ihandle [0], cbuffer, ISFIRST);
		if (iresult)
		{
			printf ("Error on isread - %d\n", iserrno);
			exit (1);
		}
		printf ("[%-8.8s] [%-14.14s]\n", cbuffer, cbuffer + 42);
		iresult = isdelcurr (ihandle [0]);
		if (iresult)
		{
			printf ("Error on isdelcurr - %d\n", iserrno);
			exit (1);
		}
/*
	isclose (ihandle [0]); ihandle [0] = isopen (cname, ISINOUT | ISEXCLLOCK);
*/
	}
	for (iloop = 0; iloop < FILE_COUNT; iloop++)
		isclose (ihandle [iloop]);
	return (0);
}
