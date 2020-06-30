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
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include	"isinternal.h"

static char	*pcwritebuffer;

/* Local functions */

static int
irowdelete (const int ihandle, off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	int		ikeynumber, iresult;
	off_t		tdupnumber[MAXSUBS];

	psvbptr = psvbfile[ihandle];
	/*
	 * Step 1:
	 *      Check each index for existance of trownumber
	 */
	for (ikeynumber = 0; ikeynumber < psvbptr->inkeys; ikeynumber++) {
		if (psvbptr->pskeydesc[ikeynumber]->k_nparts == 0) {
			continue;
		}
		if (ivbkeylocaterow (ihandle, ikeynumber, trownumber)) {
			iserrno = EBADFILE;
			return -1;
		}
		tdupnumber[ikeynumber] = psvbptr->pskeycurr[ikeynumber]->tdupnumber;
	}

	/*
	 * Step 2:
	 *      Perform the actual deletion from each index
	 */
	for (ikeynumber = 0; ikeynumber < psvbptr->inkeys; ikeynumber++) {
		if (psvbptr->pskeydesc[ikeynumber]->k_nparts == 0) {
			continue;
		}
		iresult = ivbkeydelete (ihandle, ikeynumber);
		if (iresult) {
			iserrno = iresult;
			return -1;
		}
	}

	return 0;
}

static int
iprocessdelete (const int ihandle, off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	int		ideleted;

	psvbptr = psvbfile[ihandle];
	if (psvbptr->iopenmode & ISTRANS) {
		iserrno = ivbdatalock (ihandle, VBWRLOCK, trownumber);
		if (iserrno) {
			return -1;
		}
	}
	iserrno = ivbdataread (ihandle, psvbptr->ppcrowbuffer,
			 &ideleted, trownumber);
	if (!iserrno && ideleted) {
		iserrno = ENOREC;
	}
	if (iserrno) {
		return -1;
	}
	if (irowdelete (ihandle, trownumber)) {
		return -1;
	}
	if (!pcwritebuffer) {
		pcwritebuffer = pvvbmalloc (MAX_RESERVED_LENGTH);
		if (!pcwritebuffer) {
			iserrno = EBADMEM;
			return -1;
		}
	}
	iserrno = ivbdatawrite (ihandle, pcwritebuffer, 1, trownumber);
	if (iserrno) {
		return -1;
	}
	if (!(psvbptr->iopenmode & ISTRANS) || ivbintrans == VBNOTRANS
	    || ivbintrans == VBCOMMIT || ivbintrans == VBROLLBACK) {
		iserrno = ivbdatafree (ihandle, trownumber);
		if (iserrno) {
			return -1;
		}
	}
	isrecnum = trownumber;
	if (trownumber == psvbptr->trownumber) {
		psvbptr->trownumber = 0;
	}
	ivbtransdelete (ihandle, trownumber, isreclen);	/* BUG - retval */
	return 0;
}

/* Global functions */

int
isdelete (const int ihandle, char *pcrow)
{
	struct DICTINFO	*psvbptr;
	int		iresult = 0;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}

	psvbptr = psvbfile[ihandle];
	if (psvbptr->pskeydesc[0]->k_flags & ISDUPS) {
		iserrno = ENOPRIM;
		iresult = -1;
	} else {
		vvbmakekey (psvbptr->pskeydesc[0], pcrow, ckeyvalue);
		iresult = ivbkeysearch (ihandle, ISEQUAL, 0, 0, ckeyvalue, (off_t)0);
		switch (iresult) {
		case 1:	/* Exact match */
			iresult =
			    iprocessdelete (ihandle, psvbptr->pskeycurr[0]->trownode);
			break;

		case 0:	/* LESS than */
		case 2:	/* EMPTY file */
			iserrno = ENOREC;
			iresult = -1;
			break;

		default:
			iserrno = EBADFILE;
			iresult = -1;
			break;
		}
	}

	if (iresult == 0) {
		psvbptr->iisdictlocked |= 0x02;
	}
	iresult |= ivbexit (ihandle);
	return iresult;
}

int
isdelcurr (const int ihandle)
{
	struct DICTINFO	*psvbptr;
	int		iresult = 0;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}
	psvbptr = psvbfile[ihandle];

	if (psvbptr->trownumber > 0) {
		iresult = iprocessdelete (ihandle, psvbptr->trownumber);
	} else {
		iserrno = ENOREC;
		iresult = -1;
	}

	if (iresult == 0) {
		psvbptr->iisdictlocked |= 0x02;
	}
	iresult |= ivbexit (ihandle);
	return iresult;
}

int
isdelrec (const int ihandle, vbisam_off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	int		iresult = 0;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}
	psvbptr = psvbfile[ihandle];

	if (trownumber > 0) {
		iresult = iprocessdelete (ihandle, trownumber);
	} else {
		iserrno = ENOREC;
		iresult = -1;
	}

	if (iresult == 0) {
		psvbptr->iisdictlocked |= 0x02;
	}
	iresult |= ivbexit (ihandle);
	return iresult;
}
