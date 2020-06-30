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

/* RXW
static off_t
tcountrows (const int ihandle)
{
	off_t	tnodenumber, tdatacount;
	int	inodeused;
	char	cvbnodetmp[VB_NODE_MAX];

	tnodenumber = inl_ldquad ((char *)psvbfile[ihandle]->sdictnode.cdatafree);
	tdatacount = inl_ldquad ((char *)psvbfile[ihandle]->sdictnode.cdatacount);
	while (tnodenumber) {
		if (ivbblockread (ihandle, 1, tnodenumber, cvbnodetmp)) {
			return -1;
		}
		inodeused = inl_ldint (cvbnodetmp);
		inodeused -= INTSIZE + QUADSIZE;
		tdatacount -= (inodeused / QUADSIZE);
		tnodenumber = inl_ldquad (cvbnodetmp + INTSIZE);
	}
	return tdatacount;
}
*/

/* Global functions */

int
ivbforceexit (const int ihandle)
{
	struct DICTINFO	*psvbptr;
	int		iresult = 0;
	char		cvbnodetmp[VB_NODE_MAX];

	psvbptr = psvbfile[ihandle];
	if (psvbptr->iisdictlocked & 0x02) {
		memset (cvbnodetmp, 0, VB_NODE_MAX);
		memcpy ((void *)cvbnodetmp, (void *)&psvbptr->sdictnode,
			sizeof (struct DICTNODE));
		iresult = ivbblockwrite (ihandle, 1, (off_t) 1, cvbnodetmp);
		if (iresult) {
			iserrno = EBADFILE;
		} else {
			iserrno = 0;
		}
	}
	psvbptr->iisdictlocked = 0;
	if (iresult) {
		return -1;
	}
	return 0;
}

 /* Comments:
 *	The isclose () function does not *COMPLETELY* close a table *IF* the
 *	call to isclose () occurred during a transaction.  This is to make sure
 *	that rowlocks held during a transaction are retained.  This function is
 *	the 'middle half' that performs the (possibly delayed) physical close.
 *	The 'lower half' (ivbclose3) frees up the cached stuff.
 */
int
ivbclose2 (const int ihandle)
{
	struct VBLOCK	*psrowlock;
	struct DICTINFO	*psvbptr;
	int		iloop;
	int		iindexhandle;

	psvbptr = psvbfile[ihandle];
	psvbptr->iisopen = 0;	/* It's a LIE, but so what! */
	isrelease (ihandle);
	iserrno = ivbtransclose (ihandle, psvbptr->cfilename);
	if (ivbclose (psvbptr->idatahandle)) {
		iserrno = errno;
	}
	psvbptr->idatahandle = -1;
	if (ivbclose (psvbptr->iindexhandle)) {
		iserrno = errno;
	}
	iindexhandle = psvbptr->iindexhandle;
	while (svbfile[iindexhandle].pslockhead) {
		psrowlock = svbfile[iindexhandle].pslockhead->psnext;
		vvblockfree (svbfile[iindexhandle].pslockhead);
		svbfile[iindexhandle].pslockhead = psrowlock;
	}
	svbfile[iindexhandle].pslocktail = NULL;
	psvbptr->iindexhandle = -1;
/* RXW
	psvbptr->trownumber = -1;
	psvbptr->tdupnumber = -1;
*/
	psvbptr->trownumber = 0;
	psvbptr->tdupnumber = 0;
	psvbptr->iisopen = 2;	/* Only buffers remain! */
	psvbptr->itransyet = 0;
	for (iloop = 0; iloop < MAXSUBS; iloop++) {
		psvbptr->pskeycurr[iloop] = NULL;
	}

	return (iserrno ? -1 : 0);
}

void
ivbclose3 (const int ihandle)
{
	struct DICTINFO	*psvbptr;
	int		iloop;

	psvbptr = psvbfile[ihandle];
	if (!psvbptr) {
		return;
	}
	for (iloop = 0; iloop < MAXSUBS; iloop++) {
		vvbtreeallfree (ihandle, iloop, psvbptr->pstree[iloop]);
		if (psvbptr->pskeydesc[iloop]) {
			vvbkeyunmalloc (ihandle, iloop);
			vvbfree (psvbptr->pskeydesc[iloop]);
		}
	}
	if (psvbptr->cfilename) {
		free (psvbptr->cfilename);
	}
	if (psvbptr->ppcrowbuffer) {
		free (psvbptr->ppcrowbuffer);
	}
	vvbfree (psvbptr);
	psvbfile[ihandle] = NULL;
}

int
iscleanup (void)
{
	int iloop, iresult, iresult2 = 0;

	for (iloop = 0; iloop <= ivbmaxusedhandle; iloop++)
		if (psvbfile[iloop]) {
			if (psvbfile[iloop]->iisopen == 0) {
				iresult = isclose (iloop);
				if (iresult) {
					iresult2 = iserrno;
				}
			}
			if (psvbfile[iloop]->iisopen == 1) {
				iresult = ivbclose2 (iloop);
				if (iresult) {
					iresult2 = iserrno;
				}
			}
			ivbclose3 (iloop);
		}
	if (ivblogfilehandle >= 0) {
		iresult = islogclose ();
		if (iresult) {
			iresult2 = iserrno;
		}
	}
	return iresult2;
}

int
isclose (const int ihandle)
{
	struct DICTINFO	*psvbptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	if (psvbptr->iopenmode & ISEXCLLOCK) {
		ivbforceexit (ihandle);	/* BUG retval */
	}
	psvbptr->iindexchanged = 0;
	psvbptr->iisopen = 1;
	if (!(ivbintrans == VBBEGIN || ivbintrans == VBNEEDFLUSH || ivbintrans == VBRECOVER)) {
		if (ivbclose2 (ihandle)) {
			return -1;
		}
	}
	return 0;
}

int
isfullclose (const int ihandle)
{
	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	isclose (ihandle);
	ivbclose3 (ihandle);
	return 0;
}

int
isindexinfo (const int ihandle, void *pskeydesc, const int ikeynumber)
{
	char		*pctemp;
	struct DICTINFO	*psvbptr;
	struct dictinfo	sdict;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	iserrno = EBADKEY;
	if (ikeynumber < 0 || ikeynumber > psvbptr->inkeys) {
		return -1;
	}
	iserrno = 0;
	if (ikeynumber) {
		memcpy (pskeydesc, psvbptr->pskeydesc[ikeynumber - 1],
			sizeof (struct keydesc));
		return 0;
	}

	if (ivbenter (ihandle, 1, 0)) {
		return -1;
	}

	sdict.di_nkeys = psvbptr->inkeys;
	if (psvbptr->iopenmode & ISVARLEN) {
		pctemp = (char *)&sdict.di_nkeys;
		*pctemp |= 0x80;
	}
	sdict.di_recsize = psvbptr->imaxrowlength;
	sdict.di_idxsize = psvbptr->inodesize;
/* RXW
	sdict.di_nrecords = tcountrows (ihandle);
*/
	sdict.di_nrecords = 0;
	isreclen = psvbptr->iminrowlength;
	memcpy (pskeydesc, &sdict, sizeof (struct dictinfo));

	ivbexit (ihandle);
	return 0;
}

int
isopen (const char *pcfilename, int imode)
{
	struct DICTINFO *psvbptr;
	char		*pctemp;
	struct keydesc	*pkptr;
	off_t		tnodenumber;
	int		iflags;
	int		ihandle, iindexnumber = 0, iindexpart;
	int		ikeydesclength, ilengthused, iloop, iresult;
	struct stat	sstat;
	char		tmpfname[1024];
	char		cvbnodetmp[VB_NODE_MAX];

	if ((imode & ISTRANS) && ivblogfilehandle < 0) {
		iserrno = EBADARG;	/* I'd have expected ENOLOG or ENOTRANS! */
		return -1;
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
	/*
	 * The following for loop deals with the concept of re-opening a file
	 * that was closed within the SAME transaction.  Since we were not
	 * allowed to perform the FULL close during the transaction because we
	 * needed to retain all the transactional locks, we can tremendously
	 * simplify the re-opening process.
	 */
	for (ihandle = 0; ihandle <= ivbmaxusedhandle; ihandle++) {
		psvbptr = psvbfile[ihandle];
		if (psvbptr && psvbptr->iisopen != 0) {
			if (!strcmp (psvbptr->cfilename, pcfilename)) {
				if (psvbptr->iisopen == 2) {
					sprintf (tmpfname, "%s.idx", pcfilename);
					psvbptr->iindexhandle =
					    ivbopen (tmpfname, O_RDWR | O_BINARY, 0);
					sprintf (tmpfname, "%s.dat", pcfilename);
					psvbptr->idatahandle =
					    ivbopen (tmpfname, O_RDWR | O_BINARY, 0);
					if (imode & ISEXCLLOCK) {
						iresult = ivbfileopenlock (ihandle, 2);
					} else {
						iresult = ivbfileopenlock (ihandle, 1);
					}
					if (iresult) {
						errno = EFLOCKED;
						goto open_err;
					}
				}
				psvbptr->iisopen = 0;
				psvbptr->iopenmode = imode;
				return ihandle;
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
	if (psvbfile[ihandle] == NULL) {
		errno = EBADMEM;
		goto open_err;
	}
	psvbptr = psvbfile[ihandle];
	psvbptr->cfilename = strdup (pcfilename);
	if (psvbptr->cfilename == NULL) {
		errno = EBADMEM;
		goto open_err;
	}
	psvbptr->ppcrowbuffer = pvvbmalloc (MAX_RESERVED_LENGTH);
	if (psvbptr->ppcrowbuffer == NULL) {
		errno = EBADMEM;
		goto open_err;
	}
	psvbptr->idatahandle = -1;
	psvbptr->iindexhandle = -1;
	sprintf (tmpfname, "%s.dat", pcfilename);
	if (stat (tmpfname, &sstat)) {
		errno = ENOENT;
		goto open_err;
	}
	sprintf (tmpfname, "%s.idx", pcfilename);
	if (stat (tmpfname, &sstat)) {
		errno = ENOENT;
		goto open_err;
	}
	psvbptr->iindexhandle = ivbopen (tmpfname, O_RDWR | O_BINARY, 0);
	if (psvbptr->iindexhandle < 0) {
		goto open_err;
	}
	sprintf (tmpfname, "%s.dat", pcfilename);
	psvbptr->idatahandle = ivbopen (tmpfname, O_RDWR | O_BINARY, 0);
	if (psvbptr->idatahandle < 0) {
		goto open_err;
	}
	psvbptr->iisopen = 0;

	psvbptr->inodesize = MAX_NODE_LENGTH;
	/* Get dictionary node and set node size */
	if (ivbenter (ihandle, 1, 1)) {
		errno = iserrno;
		goto open_err;
	}
	errno = EBADFILE;
#if	ISAMMODE == 1
	if (psvbptr->sdictnode.cvalidation[0] != 0x56
	    || psvbptr->sdictnode.cvalidation[1] != 0x42) {
		goto open_err;
	}
#else
	if (psvbptr->sdictnode.cvalidation[0] != -2
	    || psvbptr->sdictnode.cvalidation[1] != 0x53) {
		goto open_err;
	}
#endif
	psvbptr->inodesize = inl_ldint (psvbptr->sdictnode.cnodesize) + 1;
	psvbptr->inkeys = inl_ldint (psvbptr->sdictnode.cindexcount);
	psvbptr->iminrowlength = inl_ldint (psvbptr->sdictnode.cminrowlength);
	if (imode & ISVARLEN) {
		psvbptr->imaxrowlength = inl_ldint (psvbptr->sdictnode.cmaxrowlength);
	} else {
		psvbptr->imaxrowlength = psvbptr->iminrowlength;
	}

	errno = EROWSIZE;
	if (psvbptr->imaxrowlength && psvbptr->imaxrowlength != psvbptr->iminrowlength) {
		if (!(imode & ISVARLEN)) {
			goto open_err;
		}
	} else {
		if (imode & ISVARLEN) {
			goto open_err;
		}
	}
	psvbptr->iopenmode = imode;
	tnodenumber = inl_ldquad (psvbptr->sdictnode.cnodekeydesc);

	/* Fill in the keydesc stuff */
	while (tnodenumber) {
		iresult = ivbblockread (ihandle, 1, tnodenumber, cvbnodetmp);
		errno = EBADFILE;
		if (iresult) {
			goto open_err;
		}
		pctemp = cvbnodetmp;
		if (*(cvbnodetmp + psvbptr->inodesize - 3) != -1
		    || *(cvbnodetmp + psvbptr->inodesize - 2) != 0x7e) {
			goto open_err;
		}
		ilengthused = inl_ldint (pctemp);
		pctemp += INTSIZE;
		tnodenumber = inl_ldquad (pctemp);
		pctemp += QUADSIZE;
		ilengthused -= (INTSIZE + QUADSIZE);
		while (ilengthused > 0) {
			errno = EBADFILE;
			if (iindexnumber >= MAXSUBS) {
				goto open_err;
			}
			ikeydesclength = inl_ldint (pctemp);
			ilengthused -= ikeydesclength;
			pctemp += INTSIZE;
			psvbptr->pskeydesc[iindexnumber] = pvvbmalloc (sizeof (struct keydesc));
			pkptr = psvbptr->pskeydesc[iindexnumber];
			if (pkptr == NULL) {
				errno = EBADMEM;
				goto open_err;
			}
			pkptr->k_nparts = 0;
			pkptr->k_len = 0;
			pkptr->k_rootnode = inl_ldquad (pctemp);
/* RXW
			memcpy (&(pkptr->k_rootnode), pctemp, QUADSIZE);
*/
			pctemp += QUADSIZE;
			pkptr->k_flags = (*pctemp) * 2;
			pctemp++;
			ikeydesclength -= (QUADSIZE + INTSIZE + 1);
			iindexpart = 0;
			if (*pctemp & 0x80) {
				pkptr->k_flags |= ISDUPS;
			}
			*pctemp &= ~0x80;
			while (ikeydesclength > 0) {
				pkptr->k_nparts++;
				pkptr->k_part[iindexpart].kp_leng = inl_ldint (pctemp);
				pkptr->k_len += pkptr->k_part[iindexpart].kp_leng;
				pctemp += INTSIZE;
				pkptr->k_part[iindexpart].kp_start = inl_ldint (pctemp);
				pctemp += INTSIZE;
				pkptr->k_part[iindexpart].kp_type = *pctemp;
				pctemp++;
				ikeydesclength -= ((INTSIZE * 2) + 1);
				errno = EBADFILE;
				if (ikeydesclength < 0) {
					goto open_err;
				}
				iindexpart++;
			}
			iindexnumber++;
		}
		if (ilengthused < 0) {
			goto open_err;
		}
	}
	if (imode & ISEXCLLOCK) {
		iresult = ivbfileopenlock (ihandle, 2);
	} else {
		iresult = ivbfileopenlock (ihandle, 1);
	}
	if (iresult) {
		errno = EFLOCKED;
		goto open_err;
	}
	ivbexit (ihandle);
	iresult = isstart (ihandle, psvbptr->pskeydesc[0], 0, NULL, ISFIRST);
	if (iresult) {
		errno = iserrno;
		goto open_err;
	}

	if (ivbintrans == VBNOTRANS) {
		ivbtransopen (ihandle, pcfilename);
		psvbptr->itransyet = 1;
	}
	return ihandle;
open_err:
	ivbexit (ihandle);
	psvbptr = psvbfile[ihandle];
	if (psvbptr != NULL) {
		for (iloop = 0; iloop < MAXSUBS; iloop++) {
			if (psvbptr->pskeydesc[iloop]) {
				vvbfree (psvbptr->pskeydesc[iloop]);
			}
		}
		if (psvbptr->idatahandle != -1) {
			ivbclose (psvbptr->idatahandle);
		}
		if (psvbptr->idatahandle != -1) {
			ivbclose (psvbptr->iindexhandle);
		}
		if (psvbptr->cfilename) {
			free (psvbptr->cfilename);
		}
		if (psvbptr->ppcrowbuffer) {
			free (psvbptr->ppcrowbuffer);
		}
		vvbfree (psvbptr);
	}
	psvbfile[ihandle] = NULL;
	iserrno = errno;
	return -1;
}

int issetcollate (const int ihandle, const unsigned char *collating_sequence)
{
	struct DICTINFO *psvbptr;

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || psvbptr->iisopen) {
		iserrno = ENOTOPEN;
		return -1;
	}
	psvbptr->collating_sequence = collating_sequence;
	return 0;
}
