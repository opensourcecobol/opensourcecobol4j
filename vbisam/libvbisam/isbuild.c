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
iaddkeydescriptor (const int ihandle, struct keydesc *pskeydesc)
{
	struct DICTINFO	*psvbptr;
	char		*pcdstptr;
	off_t		theadnode, tnodenumber = 0, tnewnode;
	int		iloop, ilenkeyuncomp = 0, ilenkeydesc, inodeused;
	char		ckeydesc[INTSIZE + QUADSIZE + 1 + (NPARTS * ((INTSIZE * 2) + 1))];
	char		cvbnodetmp[VB_NODE_MAX];
	char		cvbnodetmp2[VB_NODE_MAX];

	psvbptr = psvbfile[ihandle];
	pcdstptr = ckeydesc + INTSIZE;
	/*
	 * Step 1:
	 *      Create a new 'root node' for the new index
	 */
	tnewnode = tvbnodecountgetnext (ihandle);
	if (tnewnode == -1) {
		return -1;
	}
	memset (cvbnodetmp, 0, VB_NODE_MAX);
#if	ISAMMODE == 1
	inl_stint (INTSIZE + QUADSIZE, cvbnodetmp);
	inl_stquad ((off_t)1, cvbnodetmp + INTSIZE);
#else
	inl_stint (INTSIZE, cvbnodetmp);
#endif
	iserrno = ivbblockwrite (ihandle, 1, tnewnode, cvbnodetmp);
	if (iserrno) {
		return -1;
	}
	/*
	 * Step 2:
	 *      Append the new key description to the keydesc list
	 */
	theadnode = inl_ldquad (psvbptr->sdictnode.cnodekeydesc);
	if (theadnode < 1) {
		return -1;
	}
	while (theadnode) {
		tnodenumber = theadnode;
		iserrno = ivbblockread (ihandle, 1, tnodenumber, cvbnodetmp);
		if (iserrno) {
			return -1;
		}
		theadnode = inl_ldquad (cvbnodetmp + INTSIZE);
	}
	inl_stquad (tnewnode, pcdstptr);
	pcdstptr += QUADSIZE;
	*pcdstptr = pskeydesc->k_flags / 2;
	pcdstptr++;
	for (iloop = 0; iloop < pskeydesc->k_nparts; iloop++) {
		ilenkeyuncomp += pskeydesc->k_part[iloop].kp_leng;
		inl_stint (pskeydesc->k_part[iloop].kp_leng, pcdstptr);
		if (iloop == 0 && pskeydesc->k_flags & ISDUPS) {
			*pcdstptr |= 0x80;
		}
		pcdstptr += INTSIZE;
		inl_stint (pskeydesc->k_part[iloop].kp_start, pcdstptr);
		pcdstptr += INTSIZE;
		*pcdstptr = pskeydesc->k_part[iloop].kp_type;
		pcdstptr++;
	}
	inodeused = inl_ldint (cvbnodetmp);
	ilenkeydesc = pcdstptr - ckeydesc;
	inl_stint (ilenkeydesc, ckeydesc);
	if (psvbptr->inodesize - (inodeused + 4) < ilenkeydesc) {
		tnewnode = tvbnodecountgetnext (ihandle);
		if (tnewnode == -1) {
			return -1;
		}
		memset (cvbnodetmp2, 0, VB_NODE_MAX);
		inl_stint (INTSIZE + QUADSIZE + ilenkeydesc, cvbnodetmp2);
		memcpy (cvbnodetmp2 + INTSIZE + QUADSIZE, ckeydesc, (size_t)ilenkeydesc);
		iserrno = ivbblockwrite (ihandle, 1, tnewnode, cvbnodetmp2);
		if (iserrno) {
			return -1;
		}
		inl_stquad (tnewnode, cvbnodetmp + INTSIZE);
		iserrno = ivbblockwrite (ihandle, 1, tnodenumber, cvbnodetmp);
		if (iserrno) {
			return -1;
		}
		return 0;
	}
	inl_stint (inodeused + ilenkeydesc, cvbnodetmp);
	pskeydesc->k_len = ilenkeyuncomp;
	pskeydesc->k_rootnode = tnewnode;
	memcpy (cvbnodetmp + inodeused, ckeydesc, (size_t)ilenkeydesc);
	iserrno = ivbblockwrite (ihandle, 1, tnodenumber, cvbnodetmp);
	if (iserrno) {
		return -1;
	}
	return 0;
}

static off_t
tdelkeydescriptor (const int ihandle, struct keydesc *pskeydesc, const int ikeynumber)
{
	struct DICTINFO	*psvbptr;
	char		*pcsrcptr;
	off_t		theadnode;
	int		iloop = 0;
	int		inodeused;
	int		n;
	char		cvbnodetmp[VB_NODE_MAX];

	psvbptr = psvbfile[ihandle];
	iserrno = EBADFILE;
	theadnode = inl_ldquad (psvbptr->sdictnode.cnodekeydesc);
	while (1) {
		if (!theadnode) {
			return -1;
		}
		memset (cvbnodetmp, 0, VB_NODE_MAX);
		iserrno = ivbblockread (ihandle, 1, theadnode, cvbnodetmp);
		if (iserrno) {
			return -1;
		}
		pcsrcptr = cvbnodetmp + INTSIZE + QUADSIZE;
		inodeused = inl_ldint (cvbnodetmp);
		while (pcsrcptr - cvbnodetmp < inodeused) {
			if (iloop < ikeynumber) {
				iloop++;
				pcsrcptr += inl_ldint (pcsrcptr);
				continue;
			}
			n = inl_ldint (pcsrcptr);
			inodeused -= n;
			inl_stint (inodeused, cvbnodetmp);
			memcpy (pcsrcptr, pcsrcptr + n,
				(size_t)(psvbptr->inodesize - (pcsrcptr - cvbnodetmp +
						   n)));
			iserrno = ivbblockwrite (ihandle, 1, theadnode, cvbnodetmp);
			if (iserrno) {
				return -1;
			}
			return psvbptr->pskeydesc[ikeynumber]->k_rootnode;
		}
		theadnode = inl_ldquad (cvbnodetmp + INTSIZE);
	}
	return -1;		/* Just to keep the compiler happy :) */
}

static int
idelnodes (const int ihandle, const int ikeynumber, off_t trootnode)
{
	struct DICTINFO	*psvbptr;
	char		*pcsrcptr;
	struct keydesc	*pskeydesc;
	int		iduplicate, ikeylength, icomplength = 0;
	int		inodeused, iresult = 0;
	char		clclnode[VB_NODE_MAX];

	psvbptr = psvbfile[ihandle];
	pskeydesc = psvbptr->pskeydesc[ikeynumber];
	iresult = ivbblockread (ihandle, 1, trootnode, clclnode);
	if (iresult) {
		return iresult;
	}
	/* Recurse for non-leaf nodes */
	if (*(clclnode + psvbptr->inodesize - 2)) {
		inodeused = inl_ldint (clclnode);
#if	ISAMMODE == 1
		pcsrcptr = clclnode + INTSIZE + QUADSIZE;
#else
		pcsrcptr = clclnode + INTSIZE;
#endif
		iduplicate = 0;
		while (pcsrcptr - clclnode < inodeused) {
			if (iduplicate) {
				if (!(*(pcsrcptr + QUADSIZE) & 0x80)) {
					iduplicate = 0;
				}
				*(pcsrcptr + QUADSIZE) &= ~0x80;
				iresult = idelnodes (ihandle, ikeynumber, inl_ldquad (pcsrcptr + QUADSIZE));	/* Eeek, recursion :) */
				if (iresult) {
					return iresult;
				}
				pcsrcptr += (QUADSIZE * 2);
			}
			ikeylength = pskeydesc->k_len;
			if (pskeydesc->k_flags & LCOMPRESS) {
#if	ISAMMODE == 1
				icomplength = inl_ldint (pcsrcptr);
				pcsrcptr += INTSIZE;
				ikeylength -= (icomplength - 2);
#else
				icomplength = *(pcsrcptr);
				pcsrcptr++;
				ikeylength -= (icomplength - 1);
#endif
			}
			if (pskeydesc->k_flags & TCOMPRESS) {
#if	ISAMMODE == 1
				icomplength = inl_ldint (pcsrcptr);
				pcsrcptr += INTSIZE;
				ikeylength -= (icomplength - 2);
#else
				icomplength = *pcsrcptr;
				pcsrcptr++;
				ikeylength -= (icomplength - 1);
#endif
			}
			pcsrcptr += ikeylength;
			if (pskeydesc->k_flags & ISDUPS) {
				pcsrcptr += QUADSIZE;
				if (*pcsrcptr & 0x80) {
					iduplicate = 1;
				}
			}
			iresult = idelnodes (ihandle, ikeynumber, inl_ldquad (pcsrcptr));	/* Eeek, recursion :) */
			if (iresult) {
				return iresult;
			}
			pcsrcptr += QUADSIZE;
		}
	}
	iresult = ivbnodefree (ihandle, trootnode);
	return iresult;
}

static int
imakekeysfromdata (const int ihandle, const int ikeynumber)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey;
	off_t		tdupnumber, tloop;
	int		ideleted, iresult;
	unsigned char	ckeyvalue[VB_MAX_KEYLEN];

	psvbptr = psvbfile[ihandle];
	/* Don't have to insert if the key is a NULL key! */
	if (psvbptr->pskeydesc[ikeynumber]->k_nparts == 0) {
		return 0;
	}

	for (tloop = 1; tloop < inl_ldquad (psvbptr->sdictnode.cdatacount); tloop++) {
		/*
		 * Step 1:
		 *      Read in the existing data row (Just the min rowlength)
		 */
		iserrno = ivbdataread (ihandle, psvbptr->ppcrowbuffer,
				 &ideleted, tloop);
		if (iserrno) {
			return -1;
		}
		if (ideleted) {
			continue;
		}
		/*
		 * Step 2:
		 *      Check the index for a potential ISNODUPS error (EDUPL)
		 *      Also, calculate the duplicate number as needed
		 */
		vvbmakekey (psvbptr->pskeydesc[ikeynumber], psvbptr->ppcrowbuffer, ckeyvalue);
		iresult = ivbkeysearch (ihandle, ISGREAT, ikeynumber, 0, ckeyvalue, (off_t)0);
		tdupnumber = 0;
		if (iresult >= 0 && !ivbkeyload (ihandle, ikeynumber, ISPREV, 0, &pskey)
		    && !memcmp (pskey->ckey, ckeyvalue,
				(size_t)psvbptr->pskeydesc[ikeynumber]->k_len)) {
			iserrno = EDUPL;
			if (psvbptr->pskeydesc[ikeynumber]->k_flags & ISDUPS) {
				tdupnumber = pskey->tdupnumber + 1;
			} else {
				return -1;
			}
		}

		/*
		 * Step 3:
		 * Perform the actual insertion into the index
		 */
		iresult = ivbkeyinsert (ihandle, NULL, ikeynumber, ckeyvalue, tloop,
				  tdupnumber, NULL);
		if (iresult) {
			return iresult;
		}
	}
	return 0;
}

/* Global functions */

int
isbuild (const char *pcfilename, const int imaxrowlength, struct keydesc *pskey, int imode)
{
	char		*pctemp;
	struct DICTINFO	*psvbptr;
	int		iflags, ihandle, iloop, iminrowlength;
	struct stat	sstat;
	char		cvbnodetmp[VB_NODE_MAX];
	char		tmpfname[1024];

	/* STEP 1: Sanity checks */
	if (imode & ISVARLEN) {
		iminrowlength = isreclen;
	} else {
		iminrowlength = imaxrowlength;
	}
	iflags = imode & 0x03;
	if (iflags == 3) {
		/* Cannot be BOTH ISOUTPUT and ISINOUT */
		iserrno = EBADARG;
		return -1;
	}
	if (strlen (pcfilename) > sizeof(tmpfname) - 5) {
		iserrno = EFNAME;
		return -1;
	}
	if (pskey == NULL) {
		iserrno = EBADARG;
		return -1;
	}

	/* Sanity checks passed (so far) */
	for (ihandle = 0; ihandle <= ivbmaxusedhandle; ihandle++) {
		if (psvbfile[ihandle] != NULL) {
			if (!strcmp (psvbfile[ihandle]->cfilename, pcfilename)) {
				isclose (ihandle);
				ivbclose3 (ihandle);
				break;
			}
		}
	}
	for (ihandle = 0; ; ihandle++) {
		if (ihandle > ivbmaxusedhandle) {
			if (ivbmaxusedhandle >= VB_MAX_FILES) {
				iserrno = ETOOMANY; 
				return -1;
			}
			ivbmaxusedhandle = ihandle;
			break;
		}
		if (psvbfile[ihandle] == NULL) {
			break;
		}
	}
	psvbfile[ihandle] = pvvbmalloc (sizeof (struct DICTINFO));
	psvbptr = psvbfile[ihandle];
	if (psvbptr == NULL) {
		errno = EBADMEM;
		goto build_err;
	}
	psvbptr->cfilename = strdup (pcfilename);
	if (psvbptr->cfilename == NULL) {
		errno = EBADMEM;
		goto build_err;
	}
	psvbptr->ppcrowbuffer = pvvbmalloc (MAX_RESERVED_LENGTH);
	if (psvbptr->ppcrowbuffer == NULL) {
		errno = EBADMEM;
		goto build_err;
	}
	iserrno = EBADARG;
	psvbptr->iminrowlength = iminrowlength;
	psvbptr->imaxrowlength = imaxrowlength;
	psvbptr->pskeydesc[0] = pvvbmalloc (sizeof (struct keydesc));
	if (psvbptr->pskeydesc[0] == NULL) {
		errno = EBADMEM;
		goto build_err;
	}
	memcpy (psvbptr->pskeydesc[0], pskey, sizeof (struct keydesc));
	if (ivbcheckkey (ihandle, pskey, 0, iminrowlength, 1)) {
		return -1;
	}
	sprintf (tmpfname, "%s.dat", pcfilename);
	if (!stat (tmpfname, &sstat)) {
		errno = EEXIST;
		goto build_err;
	}
	sprintf (tmpfname, "%s.idx", pcfilename);
	if (!stat (tmpfname, &sstat)) {
		errno = EEXIST;
		goto build_err;
	}
	psvbptr->iindexhandle = ivbopen (tmpfname, O_RDWR | O_CREAT | O_BINARY, 0660);
	if (psvbptr->iindexhandle < 0) {
		goto build_err;
	}
	sprintf (tmpfname, "%s.dat", pcfilename);
	psvbptr->idatahandle = ivbopen (tmpfname, O_RDWR | O_CREAT | O_BINARY, 0660);
	if (psvbptr->idatahandle < 0) {
		ivbclose (psvbptr->iindexhandle);	/* Ignore ret */
		goto build_err;
	}
	psvbptr->inkeys = 1;
	psvbptr->inodesize = MAX_NODE_LENGTH;
	psvbptr->iopenmode = imode;
	psvbptr->iisdictlocked |= 0x01;

	/* Setup root (dictionary) node (Node 1) */
	memset (cvbnodetmp, 0, VB_NODE_MAX);
	memset ((void *)&psvbptr->sdictnode, 0, sizeof (struct DICTNODE));
#if	ISAMMODE == 1
	psvbptr->sdictnode.cvalidation[0] = 'V';
	psvbptr->sdictnode.cvalidation[1] = 'B';
	psvbptr->sdictnode.crsvdperkey = 0x08;
#else
	psvbptr->sdictnode.cvalidation[0] = 0xfe;
	psvbptr->sdictnode.cvalidation[1] = 0x53;
	psvbptr->sdictnode.crsvdperkey = 0x04;
#endif
	psvbptr->sdictnode.cheaderrsvd = 0x02;
	psvbptr->sdictnode.cfooterrsvd = 0x02;
	psvbptr->sdictnode.crfu1 = 0x04;
	inl_stint (psvbptr->inodesize - 1, psvbptr->sdictnode.cnodesize);
	inl_stint (1, psvbptr->sdictnode.cindexcount);
	inl_stint (0x0704, psvbptr->sdictnode.crfu2);
	inl_stint (iminrowlength, psvbptr->sdictnode.cminrowlength);
	inl_stquad ((off_t)2, psvbptr->sdictnode.cnodekeydesc);
	inl_stquad ((off_t)0, psvbptr->sdictnode.cdatafree);
	inl_stquad ((off_t)0, psvbptr->sdictnode.cnodefree);
	inl_stquad ((off_t)0, psvbptr->sdictnode.cdatacount);
	if (pskey->k_nparts) {
		inl_stquad (3, psvbptr->sdictnode.cnodecount);
	} else {
		inl_stquad (2, psvbptr->sdictnode.cnodecount);
	}
	inl_stquad ((off_t)1, psvbptr->sdictnode.ctransnumber);
	inl_stquad ((off_t)1, psvbptr->sdictnode.cuniqueid);
	inl_stquad ((off_t)0, psvbptr->sdictnode.cnodeaudit);
	inl_stint (0x0008, psvbptr->sdictnode.clockmethod);
	if (imode & ISVARLEN) {
		inl_stint (imaxrowlength, psvbptr->sdictnode.cmaxrowlength);
	} else {
		inl_stint (0, psvbptr->sdictnode.cmaxrowlength);
	}
	memcpy (cvbnodetmp, &psvbptr->sdictnode, sizeof (struct DICTNODE));
	if (ivbblockwrite (ihandle, 1, (off_t) 1, cvbnodetmp)) {
		ivbclose (psvbptr->iindexhandle);	/* Ignore ret */
		ivbclose (psvbptr->idatahandle);	/* Ignore ret */
		if (psvbptr->cfilename) {
			free (psvbptr->cfilename);
		}
		if (psvbptr->ppcrowbuffer) {
			free (psvbptr->ppcrowbuffer);
		}
		if (psvbptr->pskeydesc[0]) {
			free (psvbptr->pskeydesc[0]);
		}
		vvbfree (psvbptr);
		psvbfile[ihandle] = NULL;
		return -1;
	}

	/* Setup first keydesc node (Node 2) */
	memset (cvbnodetmp, 0, VB_NODE_MAX);
	pctemp = cvbnodetmp;
	pctemp += INTSIZE;
	inl_stquad ((off_t)0, pctemp);	/* Next keydesc node */
	pctemp += QUADSIZE;
	/* keydesc length */
	inl_stint (INTSIZE + QUADSIZE + 1 + (((INTSIZE * 2) + 1) * pskey->k_nparts), pctemp);
	pctemp += INTSIZE;
	if (pskey->k_nparts) {
		inl_stquad ((off_t)3, pctemp);	/* Root node for this key */
	} else {
		inl_stquad ((off_t)0, pctemp);	/* Root node for this key */
	}
	pctemp += QUADSIZE;
	*pctemp = pskey->k_flags / 2;	/* Compression / Dups flags */
	pctemp++;
	for (iloop = 0; iloop < pskey->k_nparts; iloop++) {
		inl_stint (pskey->k_part[iloop].kp_leng, pctemp); /* Length */
		if (iloop == 0 && pskey->k_flags & 1) {
			*pctemp |= 0x80;
		}
		pctemp += INTSIZE;
		inl_stint (pskey->k_part[iloop].kp_start, pctemp);	/* Offset */
		pctemp += INTSIZE;
		*pctemp = pskey->k_part[iloop].kp_type;	/* Type */
		pctemp++;
	}
	inl_stint (pctemp - cvbnodetmp, cvbnodetmp);	/* Length used */
	inl_stint (0xff7e, cvbnodetmp + psvbptr->inodesize - 3);
	if (ivbblockwrite (ihandle, 1, (off_t) 2, cvbnodetmp)) {
		ivbclose (psvbptr->iindexhandle);	/* Ignore ret */
		ivbclose (psvbptr->idatahandle);	/* Ignore ret */
		if (psvbptr->cfilename) {
			free (psvbptr->cfilename);
		}
		if (psvbptr->ppcrowbuffer) {
			free (psvbptr->ppcrowbuffer);
		}
		if (psvbptr->pskeydesc[0]) {
			free (psvbptr->pskeydesc[0]);
		}
		vvbfree (psvbptr);
		psvbfile[ihandle] = NULL;
		return -1;
	}

	if (pskey->k_nparts) {
		/* Setup key root node (Node 3) */
		memset (cvbnodetmp, 0, VB_NODE_MAX);
#if	ISAMMODE == 1
		inl_stint (INTSIZE + QUADSIZE, cvbnodetmp);
		inl_stquad ((off_t)1, cvbnodetmp + INTSIZE);	/* Transaction number */
#else
		inl_stint (INTSIZE, cvbnodetmp);
#endif
		if (ivbblockwrite (ihandle, 1, (off_t) 3, cvbnodetmp)) {
			ivbclose (psvbptr->iindexhandle);	/* Ignore ret */
			ivbclose (psvbptr->idatahandle);	/* Ignore ret */
			if (psvbptr->cfilename) {
				free (psvbptr->cfilename);
			}
			if (psvbptr->ppcrowbuffer) {
				free (psvbptr->ppcrowbuffer);
			}
			if (psvbptr->pskeydesc[0]) {
				free (psvbptr->pskeydesc[0]);
			}
			vvbfree (psvbptr);
			psvbfile[ihandle] = NULL;
			return -1;
		}
	}

	psvbptr->iisopen = 0;	/* Mark it as FULLY open */
	if (imode & ISEXCLLOCK) {
		ivbfileopenlock (ihandle, 2);
	} else {
		ivbfileopenlock (ihandle, 1);
	}
	isclose (ihandle);
/* RXW - something wrong with close/open and iisopen here */
/* Really close - Why ? */
	ivbclose3 (ihandle);
	iserrno = 0;
	ivbtransbuild (pcfilename, iminrowlength, imaxrowlength, pskey, imode);
	return isopen (pcfilename, imode);
build_err:
	if (psvbfile[ihandle] != NULL) {
		if (psvbfile[ihandle]->cfilename) {
			free (psvbfile[ihandle]->cfilename);
		}
		if (psvbfile[ihandle]->ppcrowbuffer) {
			free (psvbfile[ihandle]->ppcrowbuffer);
		}
		if (psvbfile[ihandle]->pskeydesc[0]) {
			free (psvbfile[ihandle]->pskeydesc[0]);
		}
		vvbfree (psvbfile[ihandle]);
	}
	psvbfile[ihandle] = NULL;
	iserrno = errno;
	return -1;
}

int
isaddindex (const int ihandle, struct keydesc *pskeydesc)
{
	struct DICTINFO	*psvbptr;
	int		iresult, ikeynumber;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}

	iresult = -1;
	iserrno = ENOTEXCL;
	psvbptr = psvbfile[ihandle];
	if (!(psvbptr->iopenmode & ISEXCLLOCK)) {
		goto addindexexit;
	}
	iserrno = EKEXISTS;
	ikeynumber = ivbcheckkey (ihandle, pskeydesc, 1, 0, 0);
	if (ikeynumber == -1) {
		goto addindexexit;
	}
	ikeynumber = iaddkeydescriptor (ihandle, pskeydesc);
	if (ikeynumber) {
		goto addindexexit;
	}
	psvbptr->iisdictlocked |= 0x02;
	ikeynumber = ivbcheckkey (ihandle, pskeydesc, 1, 0, 0);
	if (ikeynumber < 0) {
		goto addindexexit;
	}
	for (ikeynumber = 0; ikeynumber < MAXSUBS && psvbptr->pskeydesc[ikeynumber];
	     ikeynumber++) ;
	iserrno = ETOOMANY;
	if (ikeynumber >= MAXSUBS) {
		goto addindexexit;
	}
	ikeynumber = psvbptr->inkeys;
	psvbptr->pskeydesc[ikeynumber] = pvvbmalloc (sizeof (struct keydesc));
	psvbptr->inkeys++;
	inl_stint (psvbptr->inkeys, psvbptr->sdictnode.cindexcount);
	iserrno = errno;
	if (!psvbptr->pskeydesc[ikeynumber]) {
		goto addindexexit;
	}
	memcpy (psvbptr->pskeydesc[ikeynumber], pskeydesc, sizeof (struct keydesc));
	if (imakekeysfromdata (ihandle, ikeynumber)) {
/* BUG - Handle this better! */
		iresult = iserrno;
		ivbexit (ihandle);
		isdelindex (ihandle, pskeydesc);
		iserrno = iresult;
		goto addindexexit;
	}
	iserrno = 0;
	iresult = ivbtranscreateindex (ihandle, pskeydesc);

addindexexit:
/* RXW
	iresult |= ivbforceexit (ihandle);
*/
	iresult |= ivbexit (ihandle);
	if (iresult) {
		return -1;
	}
	return 0;
}

int
isdelindex (const int ihandle, struct keydesc *pskeydesc)
{
	struct DICTINFO	*psvbptr;
	off_t		trootnode;
	int		iresult = -1, ikeynumber, iloop;

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}

	psvbptr = psvbfile[ihandle];
	if (!(psvbptr->iopenmode & ISEXCLLOCK)) {
		iserrno = ENOTEXCL;
		goto delindexexit;
	}
	ikeynumber = ivbcheckkey (ihandle, pskeydesc, 2, 0, 0);
	if (ikeynumber == -1) {
		iserrno = EKEXISTS;
		goto delindexexit;
	}
	if (!ikeynumber) {
		iserrno = EPRIMKEY;
		goto delindexexit;
	}
	trootnode = tdelkeydescriptor (ihandle, pskeydesc, ikeynumber);
	if (trootnode < 1) {
		goto delindexexit;
	}
	if (idelnodes (ihandle, ikeynumber, trootnode)) {
		goto delindexexit;
	}
	vvbfree (psvbptr->pskeydesc[ikeynumber]);
	vvbtreeallfree (ihandle, ikeynumber, psvbptr->pstree[ikeynumber]);
	vvbkeyunmalloc (ihandle, ikeynumber);
	for (iloop = ikeynumber; iloop < MAXSUBS; iloop++) {
		psvbptr->pskeydesc[iloop] = psvbptr->pskeydesc[iloop + 1];
		psvbptr->pstree[iloop] = psvbptr->pstree[iloop + 1];
		psvbptr->pskeyfree[iloop] = psvbptr->pskeyfree[iloop + 1];
		psvbptr->pskeycurr[iloop] = psvbptr->pskeycurr[iloop + 1];
	}
	psvbptr->pskeydesc[MAXSUBS - 1] = NULL;
	psvbptr->pstree[MAXSUBS - 1] = NULL;
	psvbptr->pskeyfree[MAXSUBS - 1] = NULL;
	psvbptr->pskeycurr[MAXSUBS - 1] = NULL;
	psvbptr->inkeys--;
	inl_stint (psvbptr->inkeys, psvbptr->sdictnode.cindexcount);
	psvbptr->iisdictlocked |= 0x02;
	iresult = ivbtransdeleteindex (ihandle, pskeydesc);

delindexexit:
/* RXW
	iresult |= ivbforceexit (ihandle);
*/
	iresult |= ivbexit (ihandle);
	return iresult;
}
