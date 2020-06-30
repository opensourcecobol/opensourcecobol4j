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

/* Local functions */

static int
irowupdate (const int ihandle, char *pcrow, off_t trownumber)
{
	struct VBKEY	*pskey;
	struct DICTINFO	*psvbptr;
	struct keydesc	*pskeyptr;
	off_t		tdupnumber = 0;
	int		ikeynumber, iresult;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	psvbptr = psvbfile[ihandle];
	/*
	 * Step 1:
	 *      For each index that's changing, confirm that the NEW value
	 *      doesn't conflict with an existing ISNODUPS flag.
	 */
	for (ikeynumber = 0; ikeynumber < psvbptr->inkeys; ikeynumber++) {
		pskeyptr = psvbptr->pskeydesc[ikeynumber];
		if (pskeyptr->k_nparts == 0) {
			continue;
		}
		if (pskeyptr->k_flags & ISDUPS) {
			continue;
		}
		vvbmakekey (pskeyptr, pcrow, ckeyvalue);
		iresult = ivbkeysearch (ihandle, ISGTEQ, ikeynumber, 0, ckeyvalue, (off_t)0);
		if (iresult != 1
		    || trownumber == psvbptr->pskeycurr[ikeynumber]->trownode
		    || psvbptr->pskeycurr[ikeynumber]->iisdummy) {
			continue;
		}
		iserrno = EDUPL;
		return -1;
	}

	/*
	 * Step 2:
	 *      Check each index for existance of trownumber
	 *      This 'preload' additionally helps determine which indexes change
	 */
	for (ikeynumber = 0; ikeynumber < psvbptr->inkeys; ikeynumber++) {
		pskeyptr = psvbptr->pskeydesc[ikeynumber];
		if (pskeyptr->k_nparts == 0) {
			continue;
		}
		if (ivbkeylocaterow (ihandle, ikeynumber, trownumber)) {
			iserrno = EBADFILE;
			return -1;
		}
	}

	/*
	 * Step 3:
	 *      Perform the actual deletion / insertion with each index
	 *      But *ONLY* for those indexes that have actually CHANGED!
	 */
	for (ikeynumber = 0; ikeynumber < psvbptr->inkeys; ikeynumber++) {
		pskeyptr = psvbptr->pskeydesc[ikeynumber];
		if (pskeyptr->k_nparts == 0) {
			continue;
		}
		/* pcrow is the UPDATED key! */
		vvbmakekey (pskeyptr, pcrow, ckeyvalue);
		if (!memcmp (ckeyvalue, psvbptr->pskeycurr[ikeynumber]->ckey,
			    (size_t)pskeyptr->k_len)) {
			continue;
		}
		/* If NEW key is DIFFERENT than CURRENT, remove CURRENT */
		iresult = ivbkeydelete (ihandle, ikeynumber);
		if (iresult) {
			/* An error occured.  Let's put back what we removed! */
			while (ikeynumber >= 0) {
/* BUG - We need to do SOMETHING sane here? Dunno WHAT */
				ivbkeyinsert (ihandle, NULL, ikeynumber, ckeyvalue,
					      trownumber, tdupnumber, NULL);
				ikeynumber--;
				vvbmakekey (pskeyptr,
					    psvbptr->ppcrowbuffer, ckeyvalue);
			}
			iserrno = EBADFILE;
			return -1;
		}
		iresult = ivbkeysearch (ihandle, ISGREAT, ikeynumber, 0, ckeyvalue, (off_t)0);
		tdupnumber = 0;
		if (iresult >= 0) {
			iresult = ivbkeyload (ihandle, ikeynumber, ISPREV, 1, &pskey);
			if (!iresult) {
				if (pskeyptr->k_flags & ISDUPS
				    && !memcmp (pskey->ckey, ckeyvalue,
						(size_t)pskeyptr->k_len)) {
					tdupnumber = pskey->tdupnumber + 1;
				}
				psvbptr->pskeycurr[ikeynumber] =
				    psvbptr->pskeycurr[ikeynumber]->psnext;
				psvbptr->pskeycurr[ikeynumber]->psparent->pskeycurr =
				    psvbptr->pskeycurr[ikeynumber];
			}
			iresult = ivbkeysearch (ihandle, ISGTEQ, ikeynumber, 0, ckeyvalue,
					  tdupnumber);
			iresult = ivbkeyinsert (ihandle, NULL, ikeynumber, ckeyvalue,
					  trownumber, tdupnumber, NULL);
		}
		if (iresult) {
			/* An error occured.  Let's remove what we added */
			while (ikeynumber >= 0) {
/* BUG - This is WRONG, we should re-establish what we had before! */
				/* ivbkeydelete (ihandle, ikeynumber); */
				ikeynumber--;
			}
			return iresult;
		}
	}

	iserrno = 0;
	return 0;
}

/* Global functions */

int
isrewrite (const int ihandle, char *pcrow)
{
	struct DICTINFO	*psvbptr;
	off_t		trownumber;
	int		ideleted, inewreclen, ioldreclen = 0, iresult = 0;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if ((psvbptr->iopenmode & ISVARLEN) && (isreclen > psvbptr->imaxrowlength
		|| isreclen < psvbptr->iminrowlength)) {
		iserrno = EBADARG;
		return -1;
	}

	inewreclen = isreclen;
	if (psvbptr->pskeydesc[0]->k_flags & ISDUPS) {
		iresult = -1;
		iserrno = ENOREC;
	} else {
		vvbmakekey (psvbptr->pskeydesc[0], pcrow, ckeyvalue);
		iresult = ivbkeysearch (ihandle, ISEQUAL, 0, 0, ckeyvalue, (off_t)0);
		switch (iresult) {
		case 1:	/* Exact match */
			iresult = 0;
			psvbptr->iisdictlocked |= 0x02;
			trownumber = psvbptr->pskeycurr[0]->trownode;
			if (psvbptr->iopenmode & ISTRANS) {
				iserrno = ivbdatalock (ihandle, VBWRLOCK, trownumber);
				if (iserrno) {
					iresult = -1;
					goto isrewrite_exit;
				}
			}
			iserrno = ivbdataread (ihandle, psvbptr->ppcrowbuffer,
					 &ideleted, trownumber);
			if (!iserrno && ideleted) {
				iserrno = ENOREC;
			}
			if (iserrno) {
				iresult = -1;
			} else {
				ioldreclen = isreclen;
			}

			if (!iresult) {
				iresult = irowupdate (ihandle, pcrow, trownumber);
			}
			if (!iresult) {
				isrecnum = trownumber;
				isreclen = inewreclen;
				iresult =
				    ivbdatawrite (ihandle, pcrow, 0, (off_t)isrecnum);
			}
			if (!iresult) {
				if (psvbptr->iopenmode & ISVARLEN) {
					iresult =
					    ivbtransupdate (ihandle, trownumber, ioldreclen,
							    inewreclen, pcrow);
				} else {
					iresult =
					    ivbtransupdate (ihandle, trownumber,
							    psvbptr->iminrowlength,
							    psvbptr->iminrowlength,
							    pcrow);
				}
			}
			break;

		case 0:	/* LESS than */
		case 2:	/* GREATER than */
		case 3:	/* EMPTY file */
			iserrno = ENOREC;
			iresult = -1;
			break;

		default:
			iserrno = EBADFILE;
			iresult = -1;
			break;
		}
	}

isrewrite_exit:
	iresult |= ivbexit (ihandle);
	return iresult;
}

int
isrewcurr (const int ihandle, char *pcrow)
{
	struct DICTINFO	*psvbptr;
	int		ideleted, inewreclen, ioldreclen = 0, iresult = 0;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}

	psvbptr = psvbfile[ihandle];
	if ((psvbptr->iopenmode & ISVARLEN) && (isreclen > psvbptr->imaxrowlength
		|| isreclen < psvbptr->iminrowlength)) {
		iserrno = EBADARG;
		return -1;
	}

	inewreclen = isreclen;
	if (psvbptr->trownumber > 0) {
		if (psvbptr->iopenmode & ISTRANS) {
			iserrno = ivbdatalock (ihandle, VBWRLOCK, psvbptr->trownumber);
			if (iserrno) {
				iresult = -1;
				goto isrewcurr_exit;
			}
		}
		iserrno = ivbdataread (ihandle, psvbptr->ppcrowbuffer, &ideleted,
				 psvbptr->trownumber);
		if (!iserrno && ideleted) {
			iserrno = ENOREC;
		} else {
			ioldreclen = isreclen;
		}
		if (iserrno) {
			iresult = -1;
		}
		if (!iresult) {
			iresult = irowupdate (ihandle, pcrow, psvbptr->trownumber);
		}
		if (!iresult) {
			isrecnum = psvbptr->trownumber;
			isreclen = inewreclen;
			iresult = ivbdatawrite (ihandle, pcrow, 0, (off_t)isrecnum);
		}
		if (!iresult) {
			if (psvbptr->iopenmode & ISVARLEN) {
				iresult =
				    ivbtransupdate (ihandle, psvbptr->trownumber,
						    ioldreclen, inewreclen, pcrow);
			} else {
				iresult =
				    ivbtransupdate (ihandle, psvbptr->trownumber,
						    psvbptr->iminrowlength,
						    psvbptr->iminrowlength, pcrow);
			}
		}
	}
isrewcurr_exit:
	psvbptr->iisdictlocked |= 0x02;
	iresult |= ivbexit (ihandle);
	return iresult;
}

int
isrewrec (const int ihandle, const vbisam_off_t trownumber, char *pcrow)
{
	struct DICTINFO	*psvbptr;
	int		ideleted, inewreclen, ioldreclen = 0, iresult = 0;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}

	psvbptr = psvbfile[ihandle];
	if ((psvbptr->iopenmode & ISVARLEN) && (isreclen > psvbptr->imaxrowlength
		|| isreclen < psvbptr->iminrowlength)) {
		iserrno = EBADARG;
		return -1;
	}

	inewreclen = isreclen;
	if (trownumber < 1) {
		iresult = -1;
		iserrno = ENOREC;
	} else {
		if (psvbptr->iopenmode & ISTRANS) {
			iserrno = ivbdatalock (ihandle, VBWRLOCK, trownumber);
			if (iserrno) {
				iresult = -1;
				goto isrewrec_exit;
			}
		}
		iserrno = ivbdataread (ihandle, psvbptr->ppcrowbuffer, &ideleted,
				 trownumber);
		if (!iserrno && ideleted) {
			iserrno = ENOREC;
		}
		if (iserrno) {
			iresult = -1;
		} else {
			ioldreclen = isreclen;
		}
		if (!iresult) {
			iresult = irowupdate (ihandle, pcrow, trownumber);
		}
		if (!iresult) {
			isrecnum = trownumber;
			isreclen = inewreclen;
			iresult = ivbdatawrite (ihandle, pcrow, 0, (off_t)isrecnum);
		}
		if (!iresult) {
			if (psvbptr->iopenmode & ISVARLEN) {
				iresult = ivbtransupdate (ihandle, trownumber,
						ioldreclen, inewreclen,
						pcrow);
			} else {
				iresult = ivbtransupdate (ihandle, trownumber,
						psvbptr->iminrowlength,
						psvbptr->iminrowlength, pcrow);
			}
		}
	}
isrewrec_exit:
	if (!iresult) {
		psvbptr->iisdictlocked |= 0x02;
	}
	iresult |= ivbexit (ihandle);
	return iresult;
}
