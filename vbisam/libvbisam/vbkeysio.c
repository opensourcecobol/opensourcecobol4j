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

#ifdef	WITH_LFS64
#define VB_MAX_OFF_T	(off_t)9223372036854775807LL
#else
#define VB_MAX_OFF_T	(off_t)2147483647
#endif

/* Local functions */

static int
itreeload (const int ihandle, const int ikeynumber, const int ilength,
	   unsigned char *pckeyvalue, off_t tdupnumber)
{
	struct VBKEY	*pskey;
	struct VBTREE	*pstree;
	struct DICTINFO	*psvbptr;
	struct keydesc	*pskptr;
	unsigned int	idelta, iindex;
	int		iresult = 0;

	psvbptr = psvbfile[ihandle];
	pstree = psvbptr->pstree[ikeynumber];
	pskptr = psvbptr->pskeydesc[ikeynumber];
	if (!pstree) {
		pstree = psvbtreeallocate (ihandle);
		psvbptr->pstree[ikeynumber] = pstree;
		iserrno = errno;
		if (!pstree) {
			goto treeload_exit;
		}
		pstree->iisroot = 1;
		pstree->iistof = 1;
		pstree->iiseof = 1;
		iserrno = ivbnodeload (ihandle, ikeynumber, pstree,
				 pskptr->k_rootnode, -1);
		if (iserrno) {
			vvbtreeallfree (ihandle, ikeynumber, pstree);
			psvbptr->pstree[ikeynumber] = NULL;
			psvbptr->pskeycurr[ikeynumber] = NULL;
			goto treeload_exit;
		}
	} else if (psvbptr->iindexchanged) {
		iserrno = ivbnodeload (ihandle, ikeynumber, pstree,
				 pskptr->k_rootnode, -1);
		if (iserrno) {
			vvbtreeallfree (ihandle, ikeynumber, pstree);
			psvbptr->pstree[ikeynumber] = NULL;
			psvbptr->pskeycurr[ikeynumber] = NULL;
			goto treeload_exit;
		}
	}
	iserrno = EBADFILE;
	if (pstree->tnodenumber != pskptr->k_rootnode) {
		goto treeload_exit;
	}
	pstree->iisroot = 1;
	pstree->iistof = 1;
	pstree->iiseof = 1;
	while (1) {
/* The following code takes a 'bisection' type approach for location of the */
/* key entry.  It FAR outperforms the original sequential search code. */
		idelta = 1;
		iindex = pstree->ikeysinnode;
		while (iindex) {
			idelta = idelta << 1;
			iindex = iindex >> 1;
		}
		iindex = idelta;
		while (idelta) {
			idelta = idelta >> 1;
			if (iindex > pstree->ikeysinnode) {
				iindex -= idelta;
				continue;
			}
			pstree->pskeycurr = pstree->pskeylist[iindex - 1];
			if (pstree->pskeycurr->iisdummy) {
				iresult = -1;
			} else {
				iresult = ivbkeycompare (ihandle, ikeynumber, ilength,
						pckeyvalue, pstree->pskeycurr->ckey);
			}
			if (iresult == 0) {
				if (tdupnumber > pstree->pskeycurr->tdupnumber) {
					iresult = 1;
					iindex += idelta;
					continue;
				}
				if (tdupnumber < pstree->pskeycurr->tdupnumber) {
					iresult = -1;
					iindex -= idelta;
					continue;
				}
				if (tdupnumber == pstree->pskeycurr->tdupnumber) {
					break;
				}
			}
			if (iresult < 0) {
				iindex -= idelta;
				continue;
			}
			if (iresult > 0) {
				iindex += idelta;
				continue;
			}
		}
		if (iresult > 0 && pstree->pskeycurr->psnext) {
			pstree->pskeycurr = pstree->pskeylist[iindex];
		}
		if (pstree->pskeycurr->iisdummy && pstree->pskeycurr->psprev
		    && pstree->pskeycurr->psprev->iishigh) {
			pstree->pskeycurr = pstree->pskeycurr->psprev;
		}
		iresult = ivbkeycompare (ihandle, ikeynumber, ilength, pckeyvalue,
				   pstree->pskeycurr->ckey);
		if (iresult == 0 && tdupnumber < pstree->pskeycurr->tdupnumber) {
			iresult = -1;
		}
		if (!pstree->ilevel) {
			break;	/* Exit the while loop */
		}
		if (!pstree->pskeycurr) {
			goto treeload_exit;
		}
		if (!pstree->pskeycurr->pschild || psvbptr->iindexchanged) {
			pskey = pstree->pskeycurr;
			if (!pstree->pskeycurr->pschild) {
				pskey->pschild = psvbtreeallocate (ihandle);
				iserrno = errno;
				if (!pskey->pschild) {
					goto treeload_exit;
				}
				pskey->pschild->psparent = pskey->psparent;
				if (pskey->psparent->iistof
				    && pskey == pskey->psparent->pskeyfirst) {
					pskey->pschild->iistof = 1;
				}
				if (pskey->psparent->iiseof
				    && pskey == pskey->psparent->pskeylast->psprev) {
					pskey->pschild->iiseof = 1;
				}
			}
			iserrno = ivbnodeload (ihandle, ikeynumber, pstree->pskeycurr->pschild,
					 pstree->pskeycurr->trownode, (int)pstree->ilevel);
			if (iserrno) {
				vvbtreeallfree (ihandle, ikeynumber, pskey->pschild);
				pskey->pschild = NULL;
				goto treeload_exit;
			}
			pstree->pskeycurr->psparent = pstree;
			pstree->pskeycurr->pschild->psparent = pstree;
			if (pstree->iistof && pstree->pskeycurr == pstree->pskeyfirst) {
				pstree->pskeycurr->pschild->iistof = 1;
			}
			if (pstree->iiseof && pstree->pskeycurr == pstree->pskeylast) {
				pstree->pskeycurr->pschild->iiseof = 1;
			}
		}
		pstree = pstree->pskeycurr->pschild;
	}
	/*
	 * When we get here, iresult is set depending upon whether the located
	 * key was:
	 * -1   LESS than the desired value
	 * 0    EQUAL to the desired value (including a tdupnumber match!)
	 * 1    GREATER than the desired value
	 * By simply adding one to the value, we're cool for a NON-STANDARD
	 * comparison return value.
	 */
	psvbptr->pskeycurr[ikeynumber] = pstree->pskeycurr;
	if (!pstree->pskeycurr) {
		iserrno = EBADFILE;
		return -1;
	}
	iserrno = 0;
	if (pstree->pskeycurr->iisdummy) {
		iserrno = EENDFILE;
		if (pstree->pskeycurr->psprev) {
			return 0;	/* EOF */
		} else {
			return 2;	/* Empty file! */
		}
	}
	return (iresult + 1);

treeload_exit:
	return -1;
}

/* Global functions */

void
vvbmakekey (const struct keydesc *pskeydesc, char *pcrow_buffer,
	    unsigned char *pckeyvalue)
{
	const struct keypart	*pskptr;
	char			*pcsource;
	int			ipart;

	/* Wierdly enough, a single keypart *CAN* span multiple instances */
	/* EG: Part number 1 might contain 4 long values */
	for (ipart = 0; ipart < pskeydesc->k_nparts; ipart++) {
		pskptr = &pskeydesc->k_part[ipart];
		pcsource = pcrow_buffer + pskptr->kp_start;
		memcpy (pckeyvalue, pcsource, (size_t)pskptr->kp_leng);
		pckeyvalue += pskptr->kp_leng;
	}
}

int
ivbkeycompare (const int ihandle, const int ikeynumber, int ilength,
	       unsigned char *pckey1, unsigned char *pckey2)
{
	struct keydesc	*pskeydesc;
	struct keypart	*pskptr;
	off_t		tvalue1, tvalue2;
	int		idescbias, ipart, ilengthtocompare;
	int		ivalue1, ivalue2;
	int		n;
	int		lvalue1, lvalue2;
	float		fvalue1, fvalue2;
	double		dvalue1, dvalue2;

	pskeydesc = psvbfile[ihandle]->pskeydesc[ikeynumber];
	if (ilength == 0) {
		ilength = pskeydesc->k_len;
	}
	for (ipart = 0; ilength > 0 && ipart < pskeydesc->k_nparts; ipart++) {
		pskptr = &pskeydesc->k_part[ipart];
		if (ilength >= pskptr->kp_leng) {
			ilengthtocompare = pskptr->kp_leng;
		} else {
			ilengthtocompare = ilength;
		}
		ilength -= ilengthtocompare;
		if (pskptr->kp_type & ISDESC) {
			idescbias = -1;
		} else {
			idescbias = 1;
		}
		switch (pskptr->kp_type & ~ISDESC) {
		case CHARTYPE:

			n = memcmp (pckey1, pckey2, (size_t)ilengthtocompare);
			if (n < 0) {
				return -idescbias;
			}
			if (n > 0) {
				return idescbias;
			}
			pckey1 += ilengthtocompare;
			pckey2 += ilengthtocompare;
/*
			while (ilengthtocompare--) {
				if (*pckey1 < *pckey2) {
					return -idescbias;
				}
				if (*pckey1++ > *pckey2++) {
					return idescbias;
				}
			}
*/
			break;

		case INTTYPE:
			while (ilengthtocompare >= INTSIZE) {
				ivalue1 = inl_ldint (pckey1);
				ivalue2 = inl_ldint (pckey2);
				if (ivalue1 < ivalue2) {
					return -idescbias;
				}
				if (ivalue1 > ivalue2) {
					return idescbias;
				}
				pckey1 += INTSIZE;
				pckey2 += INTSIZE;
				ilengthtocompare -= INTSIZE;
			}
			break;

		case LONGTYPE:
			while (ilengthtocompare >= LONGSIZE) {
				lvalue1 = inl_ldlong (pckey1);
				lvalue2 = inl_ldlong (pckey2);
				if (lvalue1 < lvalue2) {
					return -idescbias;
				}
				if (lvalue1 > lvalue2) {
					return idescbias;
				}
				pckey1 += LONGSIZE;
				pckey2 += LONGSIZE;
				ilengthtocompare -= LONGSIZE;
			}
			break;

		case QUADTYPE:
			while (ilengthtocompare >= QUADSIZE) {
				tvalue1 = inl_ldquad (pckey1);
				tvalue2 = inl_ldquad (pckey2);
				if (tvalue1 < tvalue2) {
					return -idescbias;
				}
				if (tvalue1 > tvalue2) {
					return idescbias;
				}
				pckey1 += QUADSIZE;
				pckey2 += QUADSIZE;
				ilengthtocompare -= QUADSIZE;
			}
			break;

		case FLOATTYPE:
			while (ilengthtocompare >= FLOATSIZE) {
				fvalue1 = ldfloat (pckey1);
				fvalue2 = ldfloat (pckey2);
				if (fvalue1 < fvalue2) {
					return -idescbias;
				}
				if (fvalue1 > fvalue2) {
					return idescbias;
				}
				pckey1 += FLOATSIZE;
				pckey2 += FLOATSIZE;
				ilengthtocompare -= FLOATSIZE;
			}
			break;

		case DOUBLETYPE:
			while (ilengthtocompare >= DOUBLESIZE) {
				dvalue1 = lddbl (pckey1);
				dvalue2 = lddbl (pckey2);
				if (dvalue1 < dvalue2) {
					return -idescbias;
				}
				if (dvalue1 > dvalue2) {
					return idescbias;
				}
				pckey1 += DOUBLESIZE;
				pckey2 += DOUBLESIZE;
				ilengthtocompare -= DOUBLESIZE;
			}
			break;

		default:
			break;
		}
	}
	return 0;
}

int
ivbkeysearch (const int ihandle, const int imode, const int ikeynumber,
	      int ilength, unsigned char *pckeyvalue, off_t tdupnumber)
{
	struct VBKEY	*pskey;
	struct keydesc	*pskeydesc;
	int		iresult;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	pskeydesc = psvbfile[ihandle]->pskeydesc[ikeynumber];
	if (ilength == 0) {
		ilength = pskeydesc->k_len;
	}
	switch (imode) {
	case ISFIRST:
		vvbkeyvalueset (0, pskeydesc, ckeyvalue);
		tdupnumber = -1;
		return itreeload (ihandle, ikeynumber, ilength, ckeyvalue, tdupnumber);

	case ISLAST:
		vvbkeyvalueset (1, pskeydesc, ckeyvalue);
		tdupnumber = VB_MAX_OFF_T;
		return itreeload (ihandle, ikeynumber, ilength, ckeyvalue, tdupnumber);

	case ISNEXT:
		iresult = ivbkeyload (ihandle, ikeynumber, ISNEXT, 1, &pskey);
		iserrno = iresult;
		if (iresult == EENDFILE) {
			iresult = 0;
		}
		if (iresult) {
			return -1;
		}
		return 1;	/* "NEXT" can NEVER be an exact match! */

	case ISPREV:
		iresult = ivbkeyload (ihandle, ikeynumber, ISPREV, 1, &pskey);
		iserrno = iresult;
		if (iresult == EENDFILE) {
			iresult = 0;
		}
		if (iresult) {
			return -1;
		}
		return 1;	/* "PREV" can NEVER be an exact match */

	case ISCURR:
		return itreeload (ihandle, ikeynumber, ilength, pckeyvalue, tdupnumber);

	case ISEQUAL:		/* Falls thru to ISGTEQ */
		tdupnumber = 0;
	case ISGTEQ:
		return itreeload (ihandle, ikeynumber, ilength, pckeyvalue, tdupnumber);

	case ISGREAT:
		tdupnumber = VB_MAX_OFF_T;
		return itreeload (ihandle, ikeynumber, ilength, pckeyvalue, tdupnumber);

	}
	/* From call sites, not possible */
	return -1;
}

int
ivbkeylocaterow (const int ihandle, const int ikeynumber, off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey;
	struct VBTREE	*pstree;
	int		iresult;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	/*
	 * Step 1:
	 *      The easy way out...
	 *      If it is already the current index pointer *AND*
	 *      the index file has remained unchanged since then,
	 *      we don't need to do anything
	 */
	psvbptr = psvbfile[ihandle];
	iresult = 1;
	pskey = psvbptr->pskeycurr[ikeynumber];
	if (pskey && pskey->trownode == trownumber) {
		pskey->psparent->pskeycurr = pskey;
		/* Position pskeycurr all the way up to the root to point at us */
		pstree = pskey->psparent;
		while (pstree->psparent) {
			for (pstree->psparent->pskeycurr = pstree->psparent->pskeyfirst;
			     pstree->psparent->pskeycurr
			     && pstree->psparent->pskeycurr->pschild != pstree;
			     pstree->psparent->pskeycurr =
			     pstree->psparent->pskeycurr->psnext) ;
			if (!pstree->psparent->pskeycurr) {
				iresult = 0;
			}
			pstree = pstree->psparent;
		}
		if (iresult) {
			return 0;
		}
	}

	/*
	 * Step 2:
	 *      It's a valid and non-deleted row.  Therefore, let's make a
	 *      contiguous key from it to search by.
	 *      Find the damn key!
	 */
	vvbmakekey (psvbptr->pskeydesc[ikeynumber], psvbptr->ppcrowbuffer, ckeyvalue);
	iresult = ivbkeysearch (ihandle, ISGTEQ, ikeynumber, 0, ckeyvalue, (off_t)0);
	if (iresult < 0 || iresult > 1) {
		iserrno = ENOREC;
		return -1;
	}
	while (psvbptr->pskeycurr[ikeynumber]->trownode != trownumber) {
		iserrno = ivbkeyload (ihandle, ikeynumber, ISNEXT, 1, &pskey);
		if (iserrno) {
			if (iserrno == EENDFILE) {
				iserrno = ENOREC;
			}
			return -1;
		}
		if (ivbkeycompare (ihandle, ikeynumber, 0, ckeyvalue,
		     psvbptr->pskeycurr[ikeynumber]->ckey)) {
			iserrno = ENOREC;
			return -1;
		}
	}
	return 0;
}

int
ivbkeyload (const int ihandle, const int ikeynumber, const int imode,
	    const int isetcurr, struct VBKEY **ppskey)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey, *pskeyhold;
	struct VBTREE	*pstree;
	int		iresult;

	psvbptr = psvbfile[ihandle];
	pskey = psvbptr->pskeycurr[ikeynumber];
	if (pskey->psparent->ilevel) {
		return EBADFILE;
	}
	switch (imode) {
	case ISPREV:
		if (pskey->psprev) {
			*ppskey = pskey->psprev;
			if (isetcurr) {
				psvbptr->pskeycurr[ikeynumber] = pskey->psprev;
				pskey->psparent->pskeycurr = pskey->psprev;
			}
			return 0;
		}
		pstree = pskey->psparent;
		if (pstree->iistof) {
			return EENDFILE;
		}
		/* Back up the tree until we find a node where there is a < */
		while (pstree->pskeycurr == pstree->pskeyfirst) {
			if (pstree->psparent) {
				pstree = pstree->psparent;
			} else {
				break;
			}
		}
		/* Back down the tree selecting the LAST valid key of each */
		pskey = pstree->pskeycurr->psprev;
		if (isetcurr) {
			pstree->pskeycurr = pstree->pskeycurr->psprev;
		}
		while (pstree->ilevel && pskey) {
			if (isetcurr) {
				pstree->pskeycurr = pskey;
			}
			if (!pskey->pschild || psvbptr->iindexchanged) {
				if (!pskey->pschild) {
					pskey->pschild = psvbtreeallocate (ihandle);
					if (!pskey->pschild) {
						return errno;
					}
					pskey->pschild->psparent = pskey->psparent;
					if (pskey->psparent->iistof
					    && pskey == pskey->psparent->pskeyfirst) {
						pskey->pschild->iistof = 1;
					}
					if (pskey->psparent->iiseof
					    && pskey == pskey->psparent->pskeylast->psprev) {
						pskey->pschild->iiseof = 1;
					}
				}
				pskeyhold = pskey;
				iresult = ivbnodeload (ihandle, ikeynumber, pskey->pschild,
						 pskey->trownode, (int)pstree->ilevel);
				if (iresult) {
					/* Ooops, make sure the tree is not corrupt */
					vvbtreeallfree (ihandle, ikeynumber,
							pskeyhold->pschild);
					pskeyhold->pschild = NULL;
					return iresult;
				}
			}
			pstree = pskey->pschild;
			/* Last key is always the dummy, so backup by one */
			pskey = pstree->pskeylast->psprev;
		}
		if (isetcurr) {
			pstree->pskeycurr = pskey;
			psvbptr->pskeycurr[ikeynumber] = pskey;
		}
		*ppskey = pskey;
		break;

	case ISNEXT:
		if (pskey->psnext && !pskey->psnext->iisdummy) {
			*ppskey = pskey->psnext;
			if (isetcurr) {
				psvbptr->pskeycurr[ikeynumber] = pskey->psnext;
				pskey->psparent->pskeycurr = pskey->psnext;
			}
			return 0;
		}
		pstree = pskey->psparent;
		if (pstree->iiseof) {
			return EENDFILE;
		}
		pstree = pstree->psparent;
		/* Back up the tree until we find a node where there is a > */
		while (1) {
			if (pstree->pskeylast->psprev != pstree->pskeycurr) {
				break;
			}
			pstree = pstree->psparent;
		}
		pskey = pstree->pskeycurr->psnext;
		if (isetcurr) {
			pstree->pskeycurr = pstree->pskeycurr->psnext;
		}
		/* Back down the tree selecting the FIRST valid key of each */
		while (pstree->ilevel) {
			if (isetcurr) {
				pstree->pskeycurr = pskey;
			}
			if (!pskey->pschild || psvbptr->iindexchanged) {
				if (!pskey->pschild) {
					pskey->pschild = psvbtreeallocate (ihandle);
					if (!pskey->pschild) {
						return errno;
					}
					pskey->pschild->psparent = pskey->psparent;
					if (pskey->psparent->iistof
					    && pskey == pskey->psparent->pskeyfirst) {
						pskey->pschild->iistof = 1;
					}
					if (pskey->psparent->iiseof
					    && pskey == pskey->psparent->pskeylast->psprev) {
						pskey->pschild->iiseof = 1;
					}
				}
				pskeyhold = pskey;
				iresult = ivbnodeload (ihandle, ikeynumber, pskey->pschild,
						 pskey->trownode, (int)pstree->ilevel);
				if (iresult) {
					/* Ooops, make sure the tree is not corrupt */
					vvbtreeallfree (ihandle, ikeynumber,
							pskeyhold->pschild);
					pskeyhold->pschild = NULL;
					return iresult;
				}
			}
			pstree = pskey->pschild;
			pskey = pstree->pskeyfirst;
		}
		if (isetcurr) {
			pstree->pskeycurr = pskey;
			psvbptr->pskeycurr[ikeynumber] = pskey;
		}
		*ppskey = pskey;
		break;

	default:
		break;
	}
	return 0;
}

void
vvbkeyvalueset (const int ihigh, struct keydesc *pskeydesc, unsigned char *pckeyvalue)
{
	struct keypart	*pskptr;
	int		ipart, iremainder;
	char		cbuffer[QUADSIZE];

	for (ipart = 0; ipart < pskeydesc->k_nparts; ipart++) {
		pskptr = &pskeydesc->k_part[ipart];
		switch (pskptr->kp_type & ~ISDESC) {
		case CHARTYPE:
			memset (pckeyvalue, ihigh ? 0xff : 0, (size_t)pskptr->kp_leng);
			pckeyvalue += pskptr->kp_leng;
			break;

		case INTTYPE:
			iremainder = pskptr->kp_leng;
			while (iremainder > 0) {
				inl_stint (ihigh ? SHRT_MAX : SHRT_MIN, pckeyvalue);
				pckeyvalue += INTSIZE;
				iremainder -= INTSIZE;
			}
			break;

		case LONGTYPE:
			iremainder = pskptr->kp_leng;
			while (iremainder > 0) {
				inl_stlong (ihigh ? LONG_MAX : LONG_MIN, pckeyvalue);
				pckeyvalue += LONGSIZE;
				iremainder -= LONGSIZE;
			}
			break;

		case QUADTYPE:
			memset (cbuffer, ihigh ? 0xff : 0, QUADSIZE);
			cbuffer[0] = ihigh ? 0x7f : 0x80;
			iremainder = pskptr->kp_leng;
			while (iremainder > 0) {
				memcpy (pckeyvalue, cbuffer, QUADSIZE);
				pckeyvalue += QUADSIZE;
				iremainder -= QUADSIZE;
			}
			break;

		case FLOATTYPE:
			iremainder = pskptr->kp_leng;
			while (iremainder > 0) {
				stfloat (ihigh ? FLT_MAX : FLT_MIN, pckeyvalue);
				pckeyvalue += FLOATSIZE;
				iremainder -= FLOATSIZE;
			}
			break;

		case DOUBLETYPE:
			iremainder = pskptr->kp_leng;
			while (iremainder > 0) {
				stdbl (ihigh ? DBL_MAX : DBL_MIN, pckeyvalue);
				pckeyvalue += DOUBLESIZE;
				iremainder -= DOUBLESIZE;
			}
			break;

		default:
			break;
		}
	}
}

int
ivbkeyinsert (const int ihandle, struct VBTREE *pstree, const int ikeynumber,
	      unsigned char *pckeyvalue, off_t trownode, off_t tdupnumber,
	      struct VBTREE *pschild)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey, *pstempkey;
	int		iposn = 0, iresult;

	psvbptr = psvbfile[ihandle];
	pskey = psvbkeyallocate (ihandle, ikeynumber);
	if (!pskey) {
		return errno;
	}
	if (!psvbptr->pskeycurr[ikeynumber]) {
		return EBADFILE;
	}
	if (!pstree) {
		pstree = psvbptr->pskeycurr[ikeynumber]->psparent;
	}
	pskey->psparent = pstree;
	pskey->pschild = pschild;
	pskey->trownode = trownode;
	pskey->tdupnumber = tdupnumber;
	pskey->iisnew = 1;
	memcpy (pskey->ckey, pckeyvalue, (size_t)psvbptr->pskeydesc[ikeynumber]->k_len);
	pskey->psnext = pstree->pskeycurr;
	pskey->psprev = pstree->pskeycurr->psprev;
	if (pstree->pskeycurr->psprev) {
		pstree->pskeycurr->psprev->psnext = pskey;
	} else {
		pstree->pskeyfirst = pskey;
	}
	pstree->pskeycurr->psprev = pskey;
	pstree->pskeycurr = pskey;
	psvbptr->pskeycurr[ikeynumber] = pskey;
	pstree->ikeysinnode = 0;
	for (pstempkey = pstree->pskeyfirst; pstempkey; pstempkey = pstempkey->psnext) {
		if (pstempkey == pskey) {
			iposn = pstree->ikeysinnode;
		}
		pstree->pskeylist[pstree->ikeysinnode] = pstempkey;
		pstree->ikeysinnode++;
	}
	iresult = ivbnodesave (ihandle, ikeynumber, pskey->psparent,
			pskey->psparent->tnodenumber, 1, iposn);
	pskey->iisnew = 0;
	return iresult;
}

int
ivbkeydelete (const int ihandle, const int ikeynumber)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey, *pskeytemp;
	struct VBTREE	*pstree, *pstreeroot;
	int		iforcerewrite = 0, iposn, iresult;

	psvbptr = psvbfile[ihandle];
	pskey = psvbptr->pskeycurr[ikeynumber];
	/*
	 * We're going to *TRY* to keep the index buffer populated!
	 * However, since it's technically feasible for the current node to be
	 * removed in it's entirety, we can only do this if there is at least 1
	 * other key in the node that's not the dummy entry.
	 * Since the current key is guaranteed to be at the LEAF node level (0),
	 * it's impossible to ever have an iishigh entry in the node.
	 */
	if (pskey->psnext && pskey->psnext->iisdummy == 0) {
		psvbptr->pskeycurr[ikeynumber] = pskey->psnext;
		pskey->psparent->pskeycurr = pskey->psnext;
	} else {
		if (pskey->psprev) {
			psvbptr->pskeycurr[ikeynumber] = pskey->psprev;
			pskey->psparent->pskeycurr = pskey->psprev;
		} else {
			psvbptr->pskeycurr[ikeynumber] = NULL;
			pskey->psparent->pskeycurr = NULL;
		}
	}
	while (1) {
		pstree = pskey->psparent;
		if (pskey->iishigh) {
			/*
			 * Handle removal of the high key in a node.
			 * Since we're modifying a key OTHER than the one we're
			 * deleting, we need a FULL node rewrite!
			 */
			if (pskey->psprev) {
				pskey->pschild = pskey->psprev->pschild;
				pskey->trownode = pskey->psprev->trownode;
				pskey->tdupnumber = pskey->psprev->tdupnumber;
				pskey = pskey->psprev;
				iforcerewrite = 1;
			} else {
				iresult = ivbnodefree (ihandle, pstree->tnodenumber);	/* BUG - didn't check iresult */
				pstree = pstree->psparent;
				vvbtreeallfree (ihandle, ikeynumber,
						pstree->pskeycurr->pschild);
				pskey = pstree->pskeycurr;
				pskey->pschild = NULL;
				continue;
			}
		}
		iposn = -1;
		pskey->psparent->ikeysinnode = 0;
		for (pskeytemp = pskey->psparent->pskeyfirst; pskeytemp;
		     pskeytemp = pskeytemp->psnext) {
			if (pskey == pskeytemp) {
				iposn = pskey->psparent->ikeysinnode;
			} else {
				pskey->psparent->pskeylist[pskey->psparent->ikeysinnode] = pskeytemp;
				pskey->psparent->ikeysinnode++;
			}
		}
		if (pskey->psprev) {
			pskey->psprev->psnext = pskey->psnext;
		} else {
			pstree->pskeyfirst = pskey->psnext;
		}
		if (pskey->psnext) {
			pskey->psnext->psprev = pskey->psprev;
		}
		pskey->psparent = NULL;
		pskey->pschild = NULL;
		vvbkeyfree (ihandle, ikeynumber, pskey);
		if (pstree->iisroot
		    && (pstree->pskeyfirst->iishigh
			|| pstree->pskeyfirst->iisdummy)) {
			pskey = pstree->pskeyfirst;
			if (!pskey->pschild) {
				pstreeroot = pstree;
			} else {
				pstreeroot = pskey->pschild;
				pskey->pschild = NULL;
			}
			if (pskey->iisdummy) {	/* EMPTY FILE! */
				return ivbnodesave (ihandle, ikeynumber, pstreeroot,
					 pstreeroot->tnodenumber, 0, 0);
			}
			iresult = ivbnodeload (ihandle, ikeynumber, pstreeroot, pskey->trownode,
					 (int)pstree->ilevel);
			if (iresult) {
				return iresult;	/* BUG Corrupt! */
			}
			iresult = ivbnodefree (ihandle, pstreeroot->tnodenumber);
			if (iresult) {
				return iresult;	/* BUG Corrupt! */
			}
			pstreeroot->tnodenumber =
			    psvbptr->pskeydesc[ikeynumber]->k_rootnode;
			pstreeroot->psparent = NULL;
			pstreeroot->iistof = 1;
			pstreeroot->iiseof = 1;
			pstreeroot->iisroot = 1;
			if (pstree != pstreeroot) {
				vvbtreeallfree (ihandle, ikeynumber, pstree);
			}
			psvbptr->pstree[ikeynumber] = pstreeroot;
			return ivbnodesave (ihandle, ikeynumber, pstreeroot,
				pstreeroot->tnodenumber, 0, 0);
		}
		if (pstree->pskeyfirst->iisdummy) {
			/* Handle removal of the last key in a node */
			iresult = ivbnodefree (ihandle, pstree->tnodenumber);
			if (iresult) {
				return iresult;	/* BUG Corrupt! */
			}
			pstree = pstree->psparent;
			vvbtreeallfree (ihandle, ikeynumber, pstree->pskeycurr->pschild);
			pstree->pskeycurr->pschild = NULL;
			pskey = pstree->pskeycurr;
			continue;
		}
		break;
	}
	if (iforcerewrite) {
		return ivbnodesave (ihandle, ikeynumber, pstree, pstree->tnodenumber, 0, 0);
	}
	return ivbnodesave (ihandle, ikeynumber, pstree, pstree->tnodenumber, -1, iposn);
}
