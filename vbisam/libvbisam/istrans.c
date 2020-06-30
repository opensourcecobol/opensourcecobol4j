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

/* Globals */
int	ivbintrans = VBNOTRANS;		/* If not zero, in a transaction */
int	ivblogfilehandle = -1;		/* Handle of the current logfile */

static struct SLOGHDR	*psvblogheader;
static pid_t		tvbpid;
static uid_t		tvbuid;
static char		cvbtransbuffer[MAX_BUFFER_LENGTH]; /* Buffer for holding transaction */

/* Local functions */

static void
vinitpiduid (void)
{
	static size_t	iinitialized = 0;

	if (iinitialized) {
		return;
	}
	iinitialized = 1;
#ifdef	_WIN32
	tvbpid = GetCurrentProcessId();
	/* No getuid ? */
	tvbuid = 1;
#else
	tvbpid = getpid ();
	tvbuid = getuid ();
#endif
}

static void
vtranshdr (const char *pctranstype)
{
	vinitpiduid ();
	psvblogheader = (struct SLOGHDR *)cvbtransbuffer;
	memcpy (psvblogheader->coperation, pctranstype, 2);
	inl_stint ((int)tvbpid, psvblogheader->cpid);	/* Assumes pid_t is short */
	inl_stint ((int)tvbuid, psvblogheader->cuid);	/* Assumes uid_t is short */
	inl_stlong (time (NULL), psvblogheader->ctime);	/* Assumes time_t is long */
	inl_stint (0, psvblogheader->crfu1);	/* BUG - WTF is this? */
}

/*
 * Name:
 *	static	int	iwritetrans (int itranslength, int irollback);
 * Arguments:
 *	int	itranslength
 *		The length of the transaction to write (exluding hdr/ftr)
 *	int	irollback
 *		0
 *			This transaction CANNOT be rolled back!
 *		1
 *			Take a wild guess!
 * Prerequisites:
 *	NONE
 * Returns:
 *	0
 *		Success
 *	ELOGWRIT
 *		Ooops, some problem occurred!
 * Problems:
 *	FUTURE
 *	When we begin to support rows > 32k, the buffer is too small.
 *	In that case, we'll need to perform SEVERAL writes and this means we
 *	will need to implement a crude locking scheme to guarantee atomicity.
 */
static int
iwritetrans (int itranslength, const int irollback)
{
	static off_t	toffset = 0;
	static int	iprevlen = 0;
	int		iresult;

	itranslength += sizeof (struct SLOGHDR) + INTSIZE;
	inl_stint (itranslength, cvbtransbuffer);
	inl_stint (itranslength, cvbtransbuffer + itranslength - INTSIZE);
	iresult = ivblock (ivblogfilehandle, (off_t)0, (off_t)1, VBWRLCKW);
	if (iresult) {
		return ELOGWRIT;
	}
	psvblogheader = (struct SLOGHDR *)cvbtransbuffer;
	if (irollback) {
		inl_stint ((int)toffset, psvblogheader->clastposn);
		inl_stint (iprevlen, psvblogheader->clastlength);
		toffset = tvblseek (ivblogfilehandle, (off_t)0, SEEK_END);
		if (toffset == -1) {
			goto writerr;
		}
		iprevlen = itranslength;
	} else {
		inl_stint (0, psvblogheader->clastposn);
		inl_stint (0, psvblogheader->clastlength);
		if (tvblseek (ivblogfilehandle, (off_t)0, SEEK_END) == -1) {
			goto writerr;
		}
	}
	if (tvbwrite (ivblogfilehandle, (void *)cvbtransbuffer, (size_t) itranslength) !=
	    (ssize_t) itranslength) {
		goto writerr;
	}
	iresult = ivblock (ivblogfilehandle, (off_t)0, (off_t)1, VBUNLOCK);
	if (iresult) {
		return ELOGWRIT;
	}
	if (ivbintrans == VBBEGIN) {
		ivbintrans = VBNEEDFLUSH;
	}
	return 0;
writerr:
	(void)ivblock (ivblogfilehandle, (off_t)0, (off_t)1, VBUNLOCK);
	return ELOGWRIT;
}

static int
iwritebegin (void)
{
	vtranshdr (VBL_BEGIN);
	return iwritetrans (0, 1);
}

/*
 * Name:
 *	static	int	idemotelocks (void);
 * Arguments:
 *	NONE
 * Prerequisites:
 *	NONE
 * Returns:
 *	0
 *		Success
 *	EBADFILE
 *		Ooops, some problem occurred!
 * Problems:
 *	See comments
 * Comments:
 *	When a transaction is completed, either with an iscommit() or an
 *	isrollback (), *ALL* held locks are released.  I'm not quite sure how
 *	valid this really is...  Perhaps only the 'transactional' locks should
 *	be released?  Or perhaps they should be retained, but demoted to a
 *	non-transactional status?  Oh well... C'est la vie!
 * Caveat:
 *	If the file is exclusively opened (ISEXCLLOCK) or has been locked with
 *	an islock () call, these locks remain in place!
 */
static int
idemotelocks (void)
{
	struct DICTINFO	*psvbptr;
	int		ihandle, iresult = 0;

	for (ihandle = 0; ihandle <= ivbmaxusedhandle; ihandle++) {
		psvbptr = psvbfile[ihandle];
		if (!psvbptr || psvbptr->iisopen == 2) {
			continue;
		}
		if (psvbptr->iopenmode & ISEXCLLOCK) {
			continue;
		}
		if (psvbptr->iisdatalocked) {
			continue;
		}
		/* Rather a carte-blanche method huh? */
		if (ivbdatalock (ihandle, VBUNLOCK, (off_t)0)) {	/* BUG Only ours? */
			iresult = -1;
		}
	}
	if (iresult) {
		iserrno = EBADFILE;
	}
	return iresult;
}

static int
ivbrollmeback (off_t toffset, const int iinrecover)
{
	char	*pcbuffer, *pcrow;
	off_t	tlength, trownumber;
	int	iloop;
	int	ierrorencountered = 0;
	int	ifoundbegin = 0;
	int	ihandle;
	int	ilocalhandle[VB_MAX_FILES + 1];
	int	isavedhandle[VB_MAX_FILES + 1];

	for (iloop = 0; iloop <= VB_MAX_FILES; iloop++) {
		if (psvbfile[iloop]) {
			ilocalhandle[iloop] = iloop;
		} else {
			ilocalhandle[iloop] = -1;
		}
		isavedhandle[iloop] = ilocalhandle[iloop];
	}
	psvblogheader = (struct SLOGHDR *)(cvbtransbuffer + INTSIZE);
	pcbuffer = cvbtransbuffer + INTSIZE + sizeof (struct SLOGHDR);
	/* Begin by reading the footer of the previous transaction */
	toffset -= INTSIZE;
	if (tvblseek (ivblogfilehandle, toffset, SEEK_SET) != toffset) {
		return EBADFILE;
	}
	if (tvbread (ivblogfilehandle, cvbtransbuffer, INTSIZE) != INTSIZE) {
		return EBADFILE;
	}
	/* Now, recurse backwards */
	while (!ifoundbegin) {
		tlength = inl_ldint (cvbtransbuffer);
		if (!tlength) {
			return EBADFILE;
		}
		toffset -= tlength;
		/* Special case: Handle where the FIRST log entry is our BW */
		if (toffset == -(INTSIZE)) {
			if (tvblseek (ivblogfilehandle, (off_t)0, SEEK_SET) != 0) {
				return EBADFILE;
			}
			if (tvbread (ivblogfilehandle, cvbtransbuffer + INTSIZE,
			     tlength - INTSIZE) != tlength - INTSIZE) {
				return EBADFILE;
			}
			if (!memcmp (psvblogheader->coperation, VBL_BEGIN, 2)) {
				break;
			}
			return EBADFILE;
		} else {
			if (toffset < INTSIZE) {
				return EBADFILE;
			}
			if (tvblseek (ivblogfilehandle, toffset, SEEK_SET) != toffset) {
				return EBADFILE;
			}
			if (tvbread (ivblogfilehandle, cvbtransbuffer, tlength) != tlength) {
				return EBADFILE;
			}
		}
		/* Is it OURS? */
		if (inl_ldint (psvblogheader->cpid) != tvbpid) {
			continue;
		}
		if (!memcmp (psvblogheader->coperation, VBL_BEGIN, 2)) {
			break;
		}
		ihandle = inl_ldint (pcbuffer);
		trownumber = inl_ldquad (pcbuffer + INTSIZE);
		if (!memcmp (psvblogheader->coperation, VBL_FILECLOSE, 2)) {
			if (ilocalhandle[ihandle] != -1 && psvbfile[ihandle]->iisopen == 0) {
				return EBADFILE;
			}
			iloop = inl_ldint (pcbuffer + INTSIZE);
			ilocalhandle[ihandle] =
			    isopen (pcbuffer + INTSIZE + INTSIZE, iloop + ISMANULOCK + ISINOUT);
			if (ilocalhandle[ihandle] == -1) {
				return ETOOMANY;
			}
		}
		if (!memcmp (psvblogheader->coperation, VBL_INSERT, 2)) {
			if (ilocalhandle[ihandle] == -1) {
				return EBADFILE;
			}
			isreclen = inl_ldint (pcbuffer + INTSIZE + QUADSIZE);
			pcrow = pcbuffer + INTSIZE + QUADSIZE + INTSIZE;
			memcpy (psvbfile[ilocalhandle[ihandle]]->ppcrowbuffer, pcrow, isreclen);
			/* BUG? - Should we READ the row first and compare it? */
			if (isdelrec (ilocalhandle[ihandle], trownumber)) {
				return iserrno;
			}
		}
		if (!memcmp (psvblogheader->coperation, VBL_UPDATE, 2)) {
			if (ilocalhandle[ihandle] == -1) {
				return EBADFILE;
			}
			isreclen = inl_ldint (pcbuffer + INTSIZE + QUADSIZE);
			pcrow = pcbuffer + INTSIZE + QUADSIZE + INTSIZE + INTSIZE;
			/* BUG? - Should we READ the row first and compare it? */
			if (isrewrec (ilocalhandle[ihandle], trownumber, pcrow)) {
				return iserrno;
			}
		}
		if (!memcmp (psvblogheader->coperation, VBL_DELETE, 2)) {
			if (ilocalhandle[ihandle] == -1) {
				return EBADFILE;
			}
			isreclen = inl_ldint (pcbuffer + INTSIZE + QUADSIZE);
			pcrow = pcbuffer + INTSIZE + QUADSIZE + INTSIZE;
			ivbenter (ilocalhandle[ihandle], 1, 0);
			psvbfile[ilocalhandle[ihandle]]->iisdictlocked |= 0x02;
			if (iinrecover
			    && ivbforcedataallocate (ilocalhandle[ihandle], trownumber)) {
				ierrorencountered = EBADFILE;
			} else {
				if (ivbwriterow (ilocalhandle[ihandle], pcrow, trownumber)) {
					ierrorencountered = EDUPL;
					ivbdatafree (ilocalhandle[ihandle], trownumber);
				}
			}
			ivbexit (ilocalhandle[ihandle]);
		}
		if (!memcmp (psvblogheader->coperation, VBL_FILEOPEN, 2)) {
			if (ilocalhandle[ihandle] == -1) {
				return EBADFILE;
			}
			isclose (ilocalhandle[ihandle]);
			ilocalhandle[ihandle] = -1;
		}
	}
	for (iloop = 0; iloop <= VB_MAX_FILES; iloop++) {
		if (isavedhandle[iloop] != -1 && psvbfile[isavedhandle[iloop]]) {
			isclose (isavedhandle[iloop]);
		}
	}
	return ierrorencountered;
}

static int
ivbrollmeforward (off_t toffset)
{
	char	*pcbuffer;
	off_t	tlength, trownumber;
	int	ifoundbegin = 0;
	int	ihandle, iloop;
	int	ilocalhandle[VB_MAX_FILES + 1], isavedhandle[VB_MAX_FILES + 1];

	vinitpiduid ();
	for (iloop = 0; iloop <= VB_MAX_FILES; iloop++) {
		if (psvbfile[iloop]) {
			ilocalhandle[iloop] = iloop;
		} else {
			ilocalhandle[iloop] = -1;
		}
		isavedhandle[iloop] = ilocalhandle[iloop];
	}
	psvblogheader = (struct SLOGHDR *)(cvbtransbuffer + INTSIZE);
	pcbuffer = cvbtransbuffer + INTSIZE + sizeof (struct SLOGHDR);
	/* Begin by reading the footer of the previous transaction */
	toffset -= INTSIZE;
	if (tvblseek (ivblogfilehandle, toffset, SEEK_SET) != toffset) {
		return EBADFILE;
	}
	if (tvbread (ivblogfilehandle, cvbtransbuffer, INTSIZE) != INTSIZE) {
		return EBADFILE;
	}
	/* Now, recurse backwards */
	while (!ifoundbegin) {
		tlength = inl_ldint (cvbtransbuffer);
		if (!tlength) {
			return EBADFILE;
		}
		toffset -= tlength;
		/* Special case: Handle where the FIRST log entry is our BW */
		if (toffset == -(INTSIZE)) {
			if (tvblseek (ivblogfilehandle, (off_t)0, SEEK_SET) != 0) {
				return EBADFILE;
			}
			if (tvbread (ivblogfilehandle, cvbtransbuffer + INTSIZE,
			     tlength - INTSIZE) != tlength - INTSIZE) {
				return EBADFILE;
			}
			if (!memcmp (psvblogheader->coperation, VBL_BEGIN, 2)) {
				break;
			}
			return EBADFILE;
		} else {
			if (toffset < INTSIZE) {
				return EBADFILE;
			}
			if (tvblseek (ivblogfilehandle, toffset, SEEK_SET) != toffset) {
				return EBADFILE;
			}
			if (tvbread (ivblogfilehandle, cvbtransbuffer, tlength) != tlength) {
				return EBADFILE;
			}
		}
		/* Is it OURS? */
		if (inl_ldint (psvblogheader->cpid) != tvbpid) {
			continue;
		}
		if (!memcmp (psvblogheader->coperation, VBL_BEGIN, 2)) {
			break;
		}
		ihandle = inl_ldint (pcbuffer);
		trownumber = inl_ldquad (pcbuffer + INTSIZE);
		if (!memcmp (psvblogheader->coperation, VBL_FILECLOSE, 2)) {
			if (ilocalhandle[ihandle] != -1) {
				return EBADFILE;
			}
			iloop = inl_ldint (pcbuffer + INTSIZE);
			ilocalhandle[ihandle] =
			    isopen (pcbuffer + INTSIZE + INTSIZE, iloop + ISMANULOCK + ISINOUT);
			if (ilocalhandle[ihandle] == -1) {
				return ETOOMANY;
			}
		}
		if (!memcmp (psvblogheader->coperation, VBL_DELETE, 2)) {
			if (ilocalhandle[ihandle] == -1) {
				return EBADFILE;
			}
			ivbenter (ilocalhandle[ihandle], 1, 0);
			psvbfile[ilocalhandle[ihandle]]->iisdictlocked |= 0x02;
			if (trownumber ==
			    inl_ldquad (psvbfile[ilocalhandle[ihandle]]->sdictnode.
					cdatacount)) {
				psvbfile[ilocalhandle[ihandle]]->iisdictlocked |= 0x02;
				inl_stquad (trownumber - 1,
					    psvbfile[ilocalhandle[ihandle]]->sdictnode.
					    cdatacount);
			} else if (ivbdatafree (ilocalhandle[ihandle], trownumber)) {
				return EBADFILE;
			}
			ivbexit (ilocalhandle[ihandle]);
		}
		if (!memcmp (psvblogheader->coperation, VBL_FILEOPEN, 2)) {
			if (ilocalhandle[ihandle] == -1) {
				return EBADFILE;
			}
			isclose (ilocalhandle[ihandle]);
		}
	}
	for (iloop = 0; iloop <= VB_MAX_FILES; iloop++) {
		if (isavedhandle[iloop] != -1 && psvbfile[isavedhandle[iloop]]) {
			isclose (isavedhandle[iloop]);
		}
	}
	return 0;
}

/* Global functions */

int
isbegin (void)
{
	if (ivblogfilehandle < 0) {
		iserrno = ELOGOPEN;
		return -1;
	}
	/* If we're already *IN* a transaction, don't start another! */
	if (ivbintrans) {
		return 0;
	}
	ivbintrans = VBBEGIN;	/* Just flag that we've BEGUN */
	return 0;
}

int
iscommit (void)
{
	struct DICTINFO	*psvbptr;
	off_t		toffset;
	int		iholdstatus = ivbintrans, iloop, iresult = 0;

	iserrno = 0;
	if (ivblogfilehandle == -1) {
		return 0;
	}
	if (!ivbintrans) {
		iserrno = ENOBEGIN;
		return -1;
	}
	vinitpiduid ();
	ivbintrans = VBCOMMIT;
	if (iholdstatus != VBBEGIN) {
		toffset = tvblseek (ivblogfilehandle, (off_t)0, SEEK_END);
		iserrno = ivbrollmeforward (toffset);
	}
	for (iloop = 0; iloop <= ivbmaxusedhandle; iloop++) {
		psvbptr = psvbfile[iloop];
		if (psvbptr && psvbptr->iisopen == 1) {
			iresult = iserrno;
			if (!ivbclose2 (iloop)) {
				iserrno = iresult;
			}
		}
	}
	/* Don't write out a 'null' transaction! */
	if (iholdstatus != VBBEGIN) {
		vtranshdr (VBL_COMMIT);
		iresult = iwritetrans (0, 1);
		if (iresult) {
			iserrno = iresult;
		}
		idemotelocks ();
	}
	ivbintrans = VBNOTRANS;
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
islogclose (void)
{
	int iresult = 0;

	if (ivbintrans == VBNEEDFLUSH) {
		if (isrollback ()) {
			iresult = iserrno;
		}
	}
	ivbintrans = VBNOTRANS;
	if (ivblogfilehandle != -1) {
		if (ivbclose (ivblogfilehandle)) {
			iresult = errno;
		}
	}
	ivblogfilehandle = -1;
	return iresult;
}

int
islogopen (const char *pcfilename)
{
	if (ivblogfilehandle != -1) {
		islogclose ();	/* Ignore the return value! */
	}
	ivblogfilehandle = ivbopen (pcfilename, O_RDWR | O_BINARY, 0);
	if (ivblogfilehandle < 0) {
		iserrno = ELOGOPEN;
		return -1;
	}
	return 0;
}

int
isrollback (void)
{
	struct DICTINFO	*psvbptr;
	off_t		toffset;
	int		iloop, iresult = 0;

	if (ivblogfilehandle < 0) {
		return 0;
	}
	if (!ivbintrans) {
		iserrno = ENOBEGIN;
		return -1;
	}
	vinitpiduid ();
	/* Don't write out a 'null' transaction! */
	for (iloop = 0; iloop <= ivbmaxusedhandle; iloop++) {
		psvbptr = psvbfile[iloop];
		if (psvbptr && psvbptr->iisopen == 1) {
			iresult = iserrno;
			if (!ivbclose2 (iloop)) {
				iserrno = iresult;
			}
			iresult = 0;
		}
	}
	if (ivbintrans == VBBEGIN) {
		return 0;
	}
	ivbintrans = VBROLLBACK;
	toffset = tvblseek (ivblogfilehandle, (off_t)0, SEEK_END);
	/* Write out the log entry */
	vtranshdr (VBL_ROLLBACK);
	iserrno = iwritetrans (0, 1);
	if (!iserrno) {
		iserrno = ivbrollmeback (toffset, 0);
	}
	idemotelocks ();
	ivbintrans = VBNOTRANS;
	if (iserrno) {
		return -1;
	}
	for (iloop = 0; iloop <= ivbmaxusedhandle; iloop++) {
		psvbptr = psvbfile[iloop];
		if (psvbptr && psvbptr->iisopen == 1) {
			if (ivbclose2 (iloop)) {
				iresult = iserrno;
			}
		}
		if (psvbptr && psvbptr->iisdictlocked & 0x04) {
			iresult |= ivbexit (iloop);
		}
	}
	return (iresult ? -1 : 0);
}

int
ivbtransbuild (const char *pcfilename, const int iminrowlen, const int imaxrowlen,
		struct keydesc *pskeydesc, const int imode)
{
	char		*pcbuffer;
	struct keypart	*pskptr;
	int		ilength = 0, ilength2, iloop;

	if (ivblogfilehandle < 0 || imode & ISNOLOG) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	vtranshdr (VBL_BUILD);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (imode, pcbuffer);
	inl_stint (iminrowlen, pcbuffer + INTSIZE);
	inl_stint (imaxrowlen, pcbuffer + (2 * INTSIZE));
	inl_stint (pskeydesc->k_flags, pcbuffer + (3 * INTSIZE));
	inl_stint (pskeydesc->k_nparts, pcbuffer + (4 * INTSIZE));
	pcbuffer += (INTSIZE * 6);
	for (iloop = 0; iloop < pskeydesc->k_nparts; iloop++) {
		pskptr = &pskeydesc->k_part[iloop];
		inl_stint (pskptr->kp_start, pcbuffer + (iloop * 3 * INTSIZE));
		inl_stint (pskptr->kp_leng,
			   pcbuffer + INTSIZE + (iloop * 3 * INTSIZE));
		inl_stint (pskptr->kp_type,
			   pcbuffer + (INTSIZE * 2) + (iloop * 3 * INTSIZE));
		ilength += pskptr->kp_leng;
	}
	inl_stint (ilength, pcbuffer - INTSIZE);
	ilength = (INTSIZE * 6) + (INTSIZE * 3 * (pskeydesc->k_nparts));
	ilength2 = strlen (pcfilename) + 1;
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR) + ilength;
	memcpy (pcbuffer, pcfilename, (size_t)ilength2);
	iserrno = iwritetrans (ilength + ilength2, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtranscreateindex (int const ihandle, struct keydesc *pskeydesc)
{
	char		*pcbuffer;
	struct keypart	*pskptr;
	int		ilength = 0, iloop;

	if (ivblogfilehandle < 0 || psvbfile[ihandle]->iopenmode & ISNOLOG) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	vtranshdr (VBL_CREINDEX);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stint (pskeydesc->k_flags, pcbuffer + INTSIZE);
	inl_stint (pskeydesc->k_nparts, pcbuffer + (2 * INTSIZE));
	pcbuffer += (INTSIZE * 4);
	for (iloop = 0; iloop < pskeydesc->k_nparts; iloop++) {
		pskptr = &pskeydesc->k_part[iloop];
		inl_stint (pskptr->kp_start, pcbuffer + (iloop * 3 * INTSIZE));
		inl_stint (pskptr->kp_leng,
			   pcbuffer + INTSIZE + (iloop * 3 * INTSIZE));
		inl_stint (pskptr->kp_type,
			   pcbuffer + (INTSIZE * 2) + (iloop * 3 * INTSIZE));
		ilength += pskptr->kp_leng;
	}
	inl_stint (ilength, pcbuffer - INTSIZE);
	ilength = (INTSIZE * 4) + (INTSIZE * 3 * (pskeydesc->k_nparts));
	iserrno = iwritetrans (ilength, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtranscluster ()
{
	/* BUG - Write ivbtranscluster */
	return 0;
}

int
ivbtransdelete (const int ihandle, off_t trownumber, int irowlength)
{
	struct DICTINFO	*psvbptr;
	char		*pcbuffer;

	psvbptr = psvbfile[ihandle];
	if (ivblogfilehandle < 0 || psvbptr->iopenmode & ISNOLOG) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	if (psvbptr->itransyet == 0) {
		ivbtransopen (ihandle, psvbptr->cfilename);
	}
	vtranshdr (VBL_DELETE);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stquad (trownumber, pcbuffer + INTSIZE);
	inl_stint (irowlength, pcbuffer + INTSIZE + QUADSIZE);
	memcpy (pcbuffer + INTSIZE + QUADSIZE + INTSIZE, psvbptr->ppcrowbuffer, (size_t)irowlength);
	irowlength += (INTSIZE * 2) + QUADSIZE;
	iserrno = iwritetrans (irowlength, 1);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtransdeleteindex (const int ihandle, struct keydesc *pskeydesc)
{
	char		*pcbuffer;
	struct keypart	*pskptr;
	int		ilength = 0, iloop;

	if (ivblogfilehandle < 0 || psvbfile[ihandle]->iopenmode & ISNOLOG) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	vtranshdr (VBL_DELINDEX);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stint (pskeydesc->k_flags, pcbuffer + INTSIZE);
	inl_stint (pskeydesc->k_nparts, pcbuffer + (2 * INTSIZE));
	pcbuffer += (INTSIZE * 4);
	for (iloop = 0; iloop < pskeydesc->k_nparts; iloop++) {
		pskptr = &pskeydesc->k_part[iloop];
		inl_stint (pskptr->kp_start, pcbuffer + (iloop * 3 * INTSIZE));
		inl_stint (pskptr->kp_leng,
			   pcbuffer + INTSIZE + (iloop * 3 * INTSIZE));
		inl_stint (pskptr->kp_type,
			   pcbuffer + (INTSIZE * 2) + (iloop * 3 * INTSIZE));
		ilength += pskptr->kp_leng;
	}
	inl_stint (ilength, pcbuffer - INTSIZE);
	ilength = (INTSIZE * 4) + (INTSIZE * 3 * (pskeydesc->k_nparts));
	iserrno = iwritetrans (ilength, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtranserase (const char *pcfilename)
{
	char	*pcbuffer;
	int	ilength;

	if (ivblogfilehandle < 0) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	vtranshdr (VBL_FILEERASE);
	ilength = strlen (pcfilename) + 1;
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	memcpy (pcbuffer, pcfilename, (size_t)ilength);
	iserrno = iwritetrans (ilength, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtransclose (const int ihandle, const char *pcfilename)
{
	struct DICTINFO	*psvbptr;
	char		*pcbuffer;
	int		ilength;

	psvbptr = psvbfile[ihandle];
	if (ivblogfilehandle < 0 || psvbptr->iopenmode & ISNOLOG) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBROLLBACK) {
		return 0;
	}
	if (psvbptr->itransyet == 0) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	vtranshdr (VBL_FILECLOSE);
	ilength = strlen (pcfilename) + 1;
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stint (psvbptr->iopenmode & ISVARLEN, pcbuffer + INTSIZE);	/* VARLEN flag! */
	memcpy (pcbuffer + INTSIZE + INTSIZE, pcfilename, (size_t)ilength);
	ilength += (INTSIZE * 2);
	iserrno = iwritetrans (ilength, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtransopen (const int ihandle, const char *pcfilename)
{
	struct DICTINFO	*psvbptr;
	char		*pcbuffer;
	int		ilength;

	psvbptr = psvbfile[ihandle];
	if (ivblogfilehandle < 0 || psvbptr->iopenmode & ISNOLOG) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	psvbptr->itransyet = 2;
	vtranshdr (VBL_FILEOPEN);
	ilength = strlen (pcfilename) + 1;
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stint (psvbptr->iopenmode & ISVARLEN, pcbuffer + INTSIZE);	/* VARLEN flag! */
	memcpy (pcbuffer + INTSIZE + INTSIZE, pcfilename, (size_t)ilength);
	ilength += (INTSIZE * 2);
	iserrno = iwritetrans (ilength, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtransinsert (const int ihandle, const off_t trownumber, int irowlength, char *pcrow)
{
	struct DICTINFO	*psvbptr;
	char		*pcbuffer;

	psvbptr = psvbfile[ihandle];
	if (ivblogfilehandle < 0 || psvbptr->iopenmode & ISNOLOG) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	if (psvbptr->itransyet == 0) {
		ivbtransopen (ihandle, psvbptr->cfilename);
	}
	vtranshdr (VBL_INSERT);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stquad (trownumber, pcbuffer + INTSIZE);
	inl_stint (irowlength, pcbuffer + INTSIZE + QUADSIZE);
	memcpy (pcbuffer + INTSIZE + QUADSIZE + INTSIZE, pcrow, (size_t)irowlength);
	irowlength += (INTSIZE * 2) + QUADSIZE;
	iserrno = iwritetrans (irowlength, 1);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtransrename (char *pcoldname, char *pcnewname)
{
	char	*pcbuffer;
	int	ilength, ilength1, ilength2;

	if (ivblogfilehandle < 0) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	vtranshdr (VBL_RENAME);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	ilength1 = strlen (pcoldname) + 1;
	ilength2 = strlen (pcnewname) + 1;
	inl_stint (ilength1, pcbuffer);
	inl_stint (ilength2, pcbuffer + INTSIZE);
	memcpy (pcbuffer + (INTSIZE * 2), pcoldname, (size_t)ilength1);
	memcpy (pcbuffer + (INTSIZE * 2) + ilength1, pcnewname, (size_t)ilength2);
	ilength = (INTSIZE * 2) + ilength1 + ilength2;
	iserrno = iwritetrans (ilength, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtranssetunique (const int ihandle, const off_t tuniqueid)
{
	struct DICTINFO	*psvbptr;
	char		*pcbuffer;

	psvbptr = psvbfile[ihandle];
	if (ivblogfilehandle < 0 || psvbptr->iopenmode & ISNOLOG) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	if (psvbptr->itransyet == 0) {
		ivbtransopen (ihandle, psvbptr->cfilename);
	}
	vtranshdr (VBL_SETUNIQUE);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stquad (tuniqueid, pcbuffer + INTSIZE);
	iserrno = iwritetrans (INTSIZE + QUADSIZE, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtransuniqueid (const int ihandle, const off_t tuniqueid)
{
	struct DICTINFO	*psvbptr;
	char		*pcbuffer;

	psvbptr = psvbfile[ihandle];
	if (ivblogfilehandle < 0 || (psvbptr->iopenmode & ISNOLOG)) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	if (psvbptr->itransyet == 0) {
		ivbtransopen (ihandle, psvbptr->cfilename);
	}
	vtranshdr (VBL_UNIQUEID);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stquad (tuniqueid, pcbuffer + INTSIZE);
	iserrno = iwritetrans (INTSIZE + QUADSIZE, 0);
	if (iserrno) {
		return -1;
	}
	return 0;
}

int
ivbtransupdate (const int ihandle, const off_t trownumber, const int ioldrowlen, const int inewrowlen, const char *pcrow)
{
	struct DICTINFO	*psvbptr;
	char		*pcbuffer;
	int		ilength;

	psvbptr = psvbfile[ihandle];
	if (ivblogfilehandle < 0 || (psvbptr->iopenmode & ISNOLOG)) {
		return 0;
	}
	/* Don't log transactions if we're in rollback / recover mode */
	if (ivbintrans > VBNEEDFLUSH) {
		return 0;
	}
	if (ivbintrans == VBBEGIN) {
		if (iwritebegin ()) {
			return -1;
		}
	}
	if (psvbptr->itransyet == 0) {
		ivbtransopen (ihandle, psvbptr->cfilename);
	}
	vtranshdr (VBL_UPDATE);
	pcbuffer = cvbtransbuffer + sizeof (struct SLOGHDR);
	inl_stint (ihandle, pcbuffer);
	inl_stquad (trownumber, pcbuffer + INTSIZE);
	inl_stint (ioldrowlen, pcbuffer + INTSIZE + QUADSIZE);
	inl_stint (inewrowlen, pcbuffer + INTSIZE + QUADSIZE + INTSIZE);
	memcpy (pcbuffer + INTSIZE + QUADSIZE + INTSIZE + INTSIZE, psvbptr->ppcrowbuffer,
		(size_t)ioldrowlen);
	memcpy (pcbuffer + INTSIZE + QUADSIZE + INTSIZE + INTSIZE + ioldrowlen, pcrow,
		(size_t)inewrowlen);
	ilength = INTSIZE + QUADSIZE + (INTSIZE * 2) + ioldrowlen + inewrowlen;
	iserrno = iwritetrans (ilength, 1);
	if (iserrno) {
		return -1;
	}
	return 0;
}
