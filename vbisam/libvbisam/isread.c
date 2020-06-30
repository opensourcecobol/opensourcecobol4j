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
istartrownumber (const int ihandle, const int imode, const int iisread)
{
	struct DICTINFO	*psvbptr;
	int ibias = 1, ideleted = 1, ilockresult = 0, iresult = 0;

	psvbptr = psvbfile[ihandle];
	switch (imode) {
	case ISFIRST:
		psvbptr->iisdisjoint = 1;
		isrecnum = 1;
		break;

	case ISLAST:
		isrecnum = inl_ldquad (psvbptr->sdictnode.cdatacount);
		ibias = -1;
		break;

	case ISNEXT:		/* Falls thru to next case! */
		if (unlikely(!iisread)) {
			iserrno = EBADARG;
			return -1;
		}
		isrecnum = psvbptr->trownumber;
		if (psvbptr->iisdisjoint) {
			ibias = 0;
			break;
		}
	case ISGREAT:		/* Falls thru to next case! */
		isrecnum++;
	case ISGTEQ:
		break;

	case ISCURR:		/* Falls thru to next case! */
		if (unlikely(!iisread)) {
			iserrno = EBADARG;
			return -1;
		}
	case ISEQUAL:
		ibias = 0;
		break;

	case ISPREV:
		if (unlikely(!iisread)) {
			iserrno = EBADARG;
			return -1;
		}
		isrecnum = psvbptr->trownumber;
		isrecnum--;
		ibias = -1;
		break;

	default:
		iserrno = EBADARG;
		return -1;
	}

	iserrno = ENOREC;
	while (ideleted) {
		if (isrecnum > 0
		    && isrecnum <= inl_ldquad (psvbptr->sdictnode.cdatacount)) {
			if (psvbptr->iopenmode & ISAUTOLOCK || imode & ISLOCK) {
				if (ivbdatalock (ihandle, imode & ISWAIT ? VBWRLCKW : VBWRLOCK, (off_t)isrecnum)) {
					ilockresult = ELOCKED;
					iresult = -1;
				}
			}
			if (!ilockresult) {
				iresult = ivbdataread (ihandle,
						 (void *)psvbptr->ppcrowbuffer,
						 &ideleted, (off_t)isrecnum);
			}
			if (iresult) {
				isrecnum = 0;
				iserrno = EBADFILE;
				return -1;
			}
		}
		if (!ideleted) {
			psvbptr->trownumber = isrecnum;
			iserrno = 0;
			return 0;
		}
		if (!ibias) {
			isrecnum = 0;
			return -1;
		}
		isrecnum += ibias;
		if (isrecnum < 1
		    || isrecnum > inl_ldquad (psvbptr->sdictnode.cdatacount)) {
			isrecnum = 0;
			return -1;
		}
	}
	return 0;
}

/* Global functions */

int
ivbcheckkey (const int ihandle, struct keydesc *pskey, const int imode,
	     int irowlength, const int iisbuild)
{
	struct DICTINFO	*psvbptr;
	struct keydesc	*pslocalkey;
	struct keypart	*pskptr;
	struct keypart	*psklptr;
	int		iloop, ipart, itype, ilocalkeylength;

	psvbptr = psvbfile[ihandle];
	if (imode) {
		irowlength = psvbptr->iminrowlength;
	}
	if (imode < 2) {
		/* Basic key validity test */
		pskey->k_len = 0;
		if (pskey->k_flags < 0 || pskey->k_flags > COMPRESS + ISDUPS) {
			goto vbcheckkey_exit;
		}
		if (pskey->k_nparts >= NPARTS || pskey->k_nparts < 0) {
			goto vbcheckkey_exit;
		}
		if (pskey->k_nparts == 0 && !iisbuild) {
			goto vbcheckkey_exit;
		}
		for (ipart = 0; ipart < pskey->k_nparts; ipart++) {
			/* Wierdly enough, a single keypart CAN span multiple instances */
			/* EG: Part number 1 might contain 4 long values */
			pskptr = &pskey->k_part[ipart];
			pskey->k_len += pskptr->kp_leng;
			if (pskey->k_len > VB_MAX_KEYLEN) {
				goto vbcheckkey_exit;
			}
			itype = pskptr->kp_type & ~ISDESC;
			switch (itype) {
			case CHARTYPE:
				break;

			case INTTYPE:
				if (pskptr->kp_leng % INTSIZE) {
					goto vbcheckkey_exit;
				}
				break;

			case LONGTYPE:
				if (pskptr->kp_leng % LONGSIZE) {
					goto vbcheckkey_exit;
				}
				break;

			case QUADTYPE:
				if (pskptr->kp_leng % QUADSIZE) {
					goto vbcheckkey_exit;
				}
				break;

			case FLOATTYPE:
				if (pskptr->kp_leng % FLOATSIZE) {
					goto vbcheckkey_exit;
				}
				break;

			case DOUBLETYPE:
				if (pskptr->kp_leng % DOUBLESIZE) {
					goto vbcheckkey_exit;
				}
				break;

			default:
				goto vbcheckkey_exit;
			}
			if (pskptr->kp_start + pskptr->kp_leng > irowlength) {
				goto vbcheckkey_exit;
			}
			if (pskptr->kp_start < 0) {
				goto vbcheckkey_exit;
			}
		}
		if (!imode) {
			return 0;
		}
	}

	/* Check whether the key already exists */
	for (iloop = 0; iloop < psvbptr->inkeys; iloop++) {
		pslocalkey = psvbptr->pskeydesc[iloop];
		if (pslocalkey->k_nparts != pskey->k_nparts) {
			continue;
		}
		ilocalkeylength = 0;
		for (ipart = 0; ipart < pslocalkey->k_nparts; ipart++) {
			pskptr = &pskey->k_part[ipart];
			psklptr = &pslocalkey->k_part[ipart];
			if (psklptr->kp_start != pskptr->kp_start) {
				break;
			}
			if (psklptr->kp_leng != pskptr->kp_leng) {
				break;
			}
			if (psklptr->kp_type != pskptr->kp_type) {
				break;
			}
			ilocalkeylength += pskptr->kp_leng;
		}
		if (ipart == pslocalkey->k_nparts) {
			pskey->k_len = ilocalkeylength;
			break;	/* found */
		}
	}
	if (iloop == psvbptr->inkeys) {
		if (imode == 2) {
			goto vbcheckkey_exit;
		}
		return iloop;
	}
	if (imode == 1) {
		goto vbcheckkey_exit;
	}
	return iloop;

vbcheckkey_exit:
	iserrno = EBADKEY;
	return -1;
}

int
isread (const int ihandle, char *pcrow, int imode)
{
	struct VBKEY	*pskey;
	struct DICTINFO	*psvbptr;
	int		ideleted = 0, ikeynumber, ilockresult = 0;
	int		ireadmode, iresult = -1;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	if (ivbenter (ihandle, 0, 0)) {
		return -1;
	}

	iserrno = EBADKEY;
	psvbptr = psvbfile[ihandle];
	ikeynumber = psvbptr->iactivekey;

	if (psvbptr->iopenmode & ISAUTOLOCK) {
		isrelease (ihandle);
	}

	ireadmode = imode & BYTEMASK;

	if (ikeynumber == -1 || !psvbptr->pskeydesc[ikeynumber]->k_nparts) {
		/*
		 * This code relies on the fact that istartrownumber will
		 * populate the global VBISAM pcrowbuffer with the fixed-length
		 * portion of the row on success.
		 */
		iresult = istartrownumber (ihandle, ireadmode, 1);
		if (!iresult) {
			memcpy (pcrow, psvbptr->ppcrowbuffer, (size_t)psvbptr->iminrowlength);
			psvbptr->iisdisjoint = 0;
		}
		goto read_exit;
	}
	iserrno = 0;
	isrecnum = 0;
	switch (ireadmode) {
	case ISFIRST:
		/* ckeyvalue is just a placeholder for ISFIRST */
		iresult = ivbkeysearch (ihandle, ISFIRST, ikeynumber, 0, ckeyvalue, (off_t)0);
		if (iresult < 0) {
			break;
		}
		if (iresult == 2) {
			iserrno = EENDFILE;
		} else {
			iresult = 0;
		}
		break;

	case ISLAST:
		/*
		 * ckeyvalue is just a placeholder for ISLAST
		 * Note that the KeySearch (ISLAST) will position the pointer onto the
		 * LAST key of the LAST tree which, by definition, is a DUMMY key
		 */
		iresult = ivbkeysearch (ihandle, ISLAST, ikeynumber, 0, ckeyvalue, (off_t)0);
		if (iresult < 0) {
			break;
		}
		if (iresult == 2) {
			iserrno = EENDFILE;
		} else {
			iresult = 0;
			iserrno = ivbkeyload (ihandle, ikeynumber, ISPREV, 1, &pskey);
			if (iserrno) {
				iresult = -1;
			}
		}
		break;

	case ISEQUAL:
		vvbmakekey (psvbptr->pskeydesc[ikeynumber], pcrow, ckeyvalue);
		iresult = ivbkeysearch (ihandle, ISGTEQ, ikeynumber, 0, ckeyvalue, (off_t)0);
		if (iresult == -1) {	/* Error */
			break;
		}
		if (iresult == 1) {	/* Found it! */
			iresult = 0;
		} else {
			if (psvbptr->pskeycurr[ikeynumber]->iisdummy) {
				iresult = ivbkeyload (ihandle, ikeynumber, ISNEXT, 1, &pskey);
				if (iresult == EENDFILE) {
					iresult = -1;
					iserrno = ENOREC;
					break;
				}
				iserrno = 0;
			}
			if (memcmp (ckeyvalue, psvbptr->pskeycurr[ikeynumber]->ckey,
					(size_t)psvbptr->pskeydesc[ikeynumber]->k_len)) {
				iresult = -1;
				iserrno = ENOREC;
			} else {
				iresult = 0;
			}
		}
		break;

	case ISGREAT:
	case ISGTEQ:
		vvbmakekey (psvbptr->pskeydesc[ikeynumber], pcrow, ckeyvalue);
		iresult = ivbkeysearch (ihandle, ireadmode, ikeynumber, 0, ckeyvalue, (off_t)0);
		if (iresult < 0) {	/* Error is always error */
			break;
		}
		if (iresult == 2) {
			iserrno = EENDFILE;
			break;
		}
		if (iresult == 1) {
			iresult = 0;
		} else {
			iresult = 0;
			if (psvbptr->pskeycurr[ikeynumber]->iisdummy) {
				iserrno = EENDFILE;
				iresult = -1;
			}
		}
		break;

	case ISPREV:
		if (psvbptr->trowstart) {
			iresult = ivbkeylocaterow (ihandle, ikeynumber,
						   psvbptr->trowstart);
		} else if (psvbptr->trownumber) {
			iresult = ivbkeylocaterow (ihandle, ikeynumber,
						   psvbptr->trownumber);
		} else {
			iserrno = ENOCURR;
			iresult = -1;
			break;
		}
		if (iresult) {
			iserrno = ENOCURR;
		} else {
			iresult = 0;
			iserrno = ivbkeyload (ihandle, ikeynumber, ISPREV, 1, &pskey);
			if (iserrno) {
				iresult = -1;
			}
		}
		break;

	case ISNEXT:		/* Might fall thru to ISCURR */
		if (!psvbptr->iisdisjoint) {
			if (psvbptr->trowstart) {
				iresult = ivbkeylocaterow (ihandle, ikeynumber,
						     psvbptr->trowstart);
			} else if (psvbptr->trownumber) {
				iresult = ivbkeylocaterow (ihandle, ikeynumber,
						     psvbptr->trownumber);
			} else {
				iserrno = ENOCURR;
				iresult = -1;
				break;
			}
			if (iresult) {
				iserrno = EENDFILE;
			} else {
				iresult = 0;
				iserrno = ivbkeyload (ihandle, ikeynumber, ISNEXT, 1, &pskey);
				if (iserrno) {
					iresult = -1;
				}
			}
			break;	/* Exit the switch case */
		}
	case ISCURR:
		if (psvbptr->trowstart) {
			iresult = ivbkeylocaterow (ihandle, ikeynumber, psvbptr->trowstart);
		} else if (psvbptr->trownumber) {
			iresult = ivbkeylocaterow (ihandle, ikeynumber,
					     psvbptr->trownumber);
		} else {
			iserrno = ENOCURR;
			iresult = -1;
			break;
		}
		if (iresult) {
			iserrno = ENOCURR;
		}
		break;

	default:
		iserrno = EBADARG;
		iresult = -1;
	}
	/* By the time we get here, we're done with index positioning... */
	/* If iresult == 0 then we have a valid row to read in */
	if (!iresult) {
		psvbptr->trowstart = 0;
		if (imode & ISLOCK || psvbptr->iopenmode & ISAUTOLOCK) {
			if (ivbdatalock
			    (ihandle, imode & ISWAIT ? VBWRLCKW : VBWRLOCK,
			     psvbptr->pskeycurr[ikeynumber]->trownode)) {
				iresult = -1;
				iserrno = ilockresult = ELOCKED;
			}
		}
		if (!ilockresult) {
			iresult = ivbdataread (ihandle, pcrow, &ideleted,
					 psvbptr->pskeycurr[ikeynumber]->trownode);
		}
		if (!iresult && (!ilockresult || (imode & ISSKIPLOCK && iserrno == ELOCKED))) {
			isrecnum = psvbptr->pskeycurr[ikeynumber]->trownode;
			psvbptr->trownumber = isrecnum;
			psvbptr->iisdisjoint = 0;
		}
	}

read_exit:
	psvbptr->iisdictlocked |= 0x04;
	ivbexit (ihandle);
	return iresult;
}

int
isstart (const int ihandle, struct keydesc *pskeydesc, int ilength, char *pcrow, int imode)
{
	struct VBKEY	*pskey;
	struct DICTINFO	*psvbptr;
	int		ikeynumber, iresult;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];
	unsigned char	ckeyvalue2[VB_MAX_KEYLEN];

	if (ivbenter (ihandle, 0, 0)) {
		return -1;
	}

	psvbptr = psvbfile[ihandle];
	ikeynumber = ivbcheckkey (ihandle, pskeydesc, 2, 0, 0);
	iresult = -1;
	if (ikeynumber == -1 && pskeydesc->k_nparts) {
		goto startexit;
	}
	if (ilength < 1 || ilength > psvbptr->pskeydesc[ikeynumber]->k_len) {
		ilength = pskeydesc->k_len;
	}
	psvbptr->iactivekey = ikeynumber;
	if (!(imode & ISKEEPLOCK)) {
		isrelease (ihandle);
	}
	imode &= BYTEMASK;
	if (!pskeydesc->k_nparts) {
		iresult = istartrownumber (ihandle, imode, 0);
		if (iresult && iserrno == ENOREC && imode <= ISLAST) {
			iresult = 0;
			iserrno = 0;
		}
		goto startexit;
	}
	iserrno = 0;
	switch (imode) {
	case ISFIRST:		/* ckeyvalue is just a placeholder for 1st/last */
		psvbptr->iisdisjoint = 1;
		iresult = ivbkeysearch (ihandle, ISFIRST, ikeynumber, 0, ckeyvalue, (off_t)0);
		if (iresult < 0) {
			break;
		}
		iresult = 0;
		break;

	case ISLAST:		/* ckeyvalue is just a placeholder for 1st/last */
		psvbptr->iisdisjoint = 0;
		iresult = ivbkeysearch (ihandle, ISLAST, ikeynumber, 0, ckeyvalue, (off_t)0);
		if (iresult < 0 || iresult > 2) {
			iresult = -1;
			break;
		}
		iserrno = ivbkeyload (ihandle, ikeynumber, ISPREV, 1, &pskey);
		if (iserrno) {
			iresult = -1;
		}
		break;

	case ISEQUAL:
		psvbptr->iisdisjoint = 1;
		vvbmakekey (psvbptr->pskeydesc[ikeynumber], pcrow, ckeyvalue2);
		memset (ckeyvalue, 0, sizeof (ckeyvalue));
		memcpy (ckeyvalue, ckeyvalue2, (size_t)ilength);
		if (ilength < pskeydesc->k_len) {
			iresult = ivbkeysearch (ihandle, ISGTEQ, ikeynumber, 0, ckeyvalue, (off_t)0);
		} else {
			iresult = ivbkeysearch (ihandle, ISEQUAL, ikeynumber, ilength, ckeyvalue, (off_t)0);
		}
		iserrno = EBADFILE;
		if (iresult == -1) {	/* Error */
			break;
		}
		/* Map EQUAL onto OK and LESS THAN onto OK if the basekey is == */
		if (iresult == 1) {
			iresult = 0;
		} else if (iresult == 0
			 && memcmp (ckeyvalue,
				    psvbptr->pskeycurr[psvbptr->iactivekey]->ckey,
				    (size_t)ilength)) {
			iserrno = ENOREC;
			iresult = -1;
		}
		break;

	case ISGREAT:
	case ISGTEQ:
		psvbptr->iisdisjoint = 1;
		vvbmakekey (psvbptr->pskeydesc[ikeynumber], pcrow, ckeyvalue2);
		if (ilength < pskeydesc->k_len && imode == ISGREAT) {
			memset (ckeyvalue, 255, sizeof (ckeyvalue));
		} else {
			memset (ckeyvalue, 0, sizeof (ckeyvalue));
		}
		memcpy (ckeyvalue, ckeyvalue2, (size_t)ilength);
		iresult = ivbkeysearch (ihandle, imode, ikeynumber, 0, ckeyvalue, (off_t)0);
		if (iserrno == EENDFILE) {
			iserrno = ENOREC;
			iresult = -1;
			break;
		}
		if (iresult < 0) {	/* Error is always error */
			break;
		}
		if (iresult < 2) {
			iresult = 0;
			break;
		}
		iserrno = EENDFILE;
		iresult = -1;
		break;

	default:
		iserrno = EBADARG;
		iresult = -1;
	}
	if (!iresult) {
		iserrno = 0;
		isrecnum = psvbptr->pskeycurr[ikeynumber]->trownode;
		psvbptr->trowstart = isrecnum;
	} else {
		psvbptr->trowstart = 0;
		iresult = -1;
	}
startexit:
	psvbptr->iisdictlocked |= 0x04;
	ivbexit (ihandle);
	return iresult;
}
