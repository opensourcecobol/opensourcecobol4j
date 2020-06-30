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

static int
irowinsert (const int ihandle, char *pcrow_buffer, off_t trownumber)
{
	struct VBKEY	*pskey;
	struct DICTINFO	*psvbptr;
	struct keydesc	*pskptr;
	off_t		tdupnumber[MAXSUBS];
	int		ikeynumber, iresult;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	psvbptr = psvbfile[ihandle];
	/*
	 * Step 1:
	 *      Check each index for a potential ISNODUPS error (EDUPL)
	 *      Also, calculate the duplicate number as needed
	 */
	for (ikeynumber = 0; ikeynumber < psvbptr->inkeys; ikeynumber++) {
		pskptr = psvbptr->pskeydesc[ikeynumber];
		if (pskptr->k_nparts == 0) {
			continue;
		}
		vvbmakekey (psvbptr->pskeydesc[ikeynumber], pcrow_buffer, ckeyvalue);
		iresult = ivbkeysearch (ihandle, ISGREAT, ikeynumber, 0, ckeyvalue, (off_t)0);
		tdupnumber[ikeynumber] = 0;
		if (iresult >= 0 && !ivbkeyload (ihandle, ikeynumber, ISPREV, 0, &pskey)
		    && !memcmp (pskey->ckey, ckeyvalue, (size_t)pskptr->k_len)) {
			iserrno = EDUPL;
			if (pskptr->k_flags & ISDUPS) {
				tdupnumber[ikeynumber] = pskey->tdupnumber + 1;
			} else {
				return -1;
			}
		}
		iresult = ivbkeysearch (ihandle, ISGTEQ, ikeynumber, 0, ckeyvalue,
				  tdupnumber[ikeynumber]);
	}

	/* Step 2: Perform the actual insertion into each index */
	for (ikeynumber = 0; ikeynumber < psvbptr->inkeys; ikeynumber++) {
		if (psvbptr->pskeydesc[ikeynumber]->k_nparts == 0) {
			continue;
		}
		vvbmakekey (psvbptr->pskeydesc[ikeynumber], pcrow_buffer, ckeyvalue);
		iresult = ivbkeyinsert (ihandle, NULL, ikeynumber, ckeyvalue,
				trownumber, tdupnumber[ikeynumber], NULL);
		if (iresult) {
/* BUG - do something SANE here */
			/* Eeek, an error occured.  Let's remove what we added */
			/* while (ikeynumber >= 0) */
			/* { */
			/* ivbkeydelete (ihandle, ikeynumber); */
			/* ikeynumber--; */
			/* } */
			return iresult;
		}
	}

	return 0;
}

/* Global functions */

int
ivbwriterow (const int ihandle, char *pcrow, const off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	int		iresult = 0;

	psvbptr = psvbfile[ihandle];
	isrecnum = trownumber;
	if (psvbptr->iopenmode & ISTRANS) {
		iserrno = ivbdatalock (ihandle, VBWRLOCK, trownumber);
		if (iserrno) {
			return -1;
		}
	}
	iresult = irowinsert (ihandle, pcrow, trownumber);
	if (!iresult) {
		iserrno = 0;
		psvbptr->tvarlennode = 0;	/* Stop it from removing */
		iresult = ivbdatawrite (ihandle, (void *)pcrow, 0, trownumber);
		if (iresult) {
			iserrno = iresult;
			if (psvbptr->iopenmode & ISTRANS) {
				ivbdatalock (ihandle, VBUNLOCK, trownumber);
			}
			return -1;
		}
		if (psvbptr->iopenmode & ISVARLEN) {
			iresult = ivbtransinsert (ihandle, trownumber,
					isreclen, pcrow);
		} else {
			iresult = ivbtransinsert (ihandle, trownumber,
					psvbptr->iminrowlength, pcrow);
		}
	}
	return iresult;
}

int
iswrcurr (const int ihandle, char *pcrow)
{
	struct DICTINFO	*psvbptr;
	off_t	trownumber;
	int	iresult;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}
	psvbptr = psvbfile[ihandle];

	if ((psvbptr->iopenmode & ISVARLEN) && (isreclen > psvbptr->imaxrowlength
		|| isreclen < psvbptr->iminrowlength)) {
		iserrno = EBADARG;
		return -1;
	}

	trownumber = tvbdataallocate (ihandle);
	if (trownumber == -1) {
		return -1;
	}

	iresult = ivbwriterow (ihandle, pcrow, trownumber);
	if (!iresult) {
		psvbptr->trownumber = trownumber;
	} else {
		ivbdatafree (ihandle, trownumber);
	}

	ivbexit (ihandle);
	return iresult;
}

int
iswrite (const int ihandle, char *pcrow)
{
	struct DICTINFO	*psvbptr;
	off_t trownumber;
	int iresult, isaveerror;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}
	psvbptr = psvbfile[ihandle];

	if ((psvbptr->iopenmode & ISVARLEN) && (isreclen > psvbptr->imaxrowlength
		|| isreclen < psvbptr->iminrowlength)) {
		iserrno = EBADARG;
		return -1;
	}

	trownumber = tvbdataallocate (ihandle);
	if (trownumber == -1) {
		return -1;
	}

	iresult = ivbwriterow (ihandle, pcrow, trownumber);
	if (iresult) {
		isaveerror = iserrno;
		ivbdatafree (ihandle, trownumber);
		iserrno = isaveerror;
	}

	ivbexit (ihandle);
	return iresult;
}
