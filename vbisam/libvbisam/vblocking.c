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

#define	IVBBUFFERLEVEL	4

/* Globals */

int	ivbmaxusedhandle = -1;		/* The highest opened file handle */

/* Local functions */

static int
ilockinsert (const int ihandle, const off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	struct VBLOCK	*psnewlock = NULL, *pslock;
	int		iindexhandle;

	psvbptr = psvbfile[ihandle];
	iindexhandle = psvbptr->iindexhandle;
	pslock = svbfile[iindexhandle].pslockhead;
	/* Insertion at head of list */
	if (pslock == NULL || trownumber < pslock->trownumber) {
		psnewlock = psvblockallocate (ihandle);
		if (psnewlock == NULL) {
			return errno;
		}
		psnewlock->ihandle = ihandle;
		psnewlock->trownumber = trownumber;
		psnewlock->psnext = pslock;
		svbfile[iindexhandle].pslockhead = psnewlock;
		if (svbfile[iindexhandle].pslocktail == NULL) {
			svbfile[iindexhandle].pslocktail = psnewlock;
		}
		return 0;
	}
	/* Insertion at tail of list */
	if (trownumber > svbfile[iindexhandle].pslocktail->trownumber) {
		psnewlock = psvblockallocate (ihandle);
		if (psnewlock == NULL) {
			return errno;
		}
		psnewlock->ihandle = ihandle;
		psnewlock->trownumber = trownumber;
		svbfile[iindexhandle].pslocktail->psnext = psnewlock;
		svbfile[iindexhandle].pslocktail = psnewlock;
		return 0;
	}
	/* Position pslock to insertion point (Keep in mind, we insert AFTER) */
	while (pslock->psnext && trownumber > pslock->psnext->trownumber) {
		pslock = pslock->psnext;
	}
	/* Already locked? */
	if (pslock->trownumber == trownumber) {
		if (pslock->ihandle == ihandle) {
			return 0;
		} else {
#ifdef	CISAMLOCKS
			return 0;
#else
			return ELOCKED;
#endif
		}
	}
	psnewlock = psvblockallocate (ihandle);
	if (psnewlock == NULL) {
		return errno;
	}
	psnewlock->ihandle = ihandle;
	psnewlock->trownumber = trownumber;
	/* Insert psnewlock AFTER pslock */
	psnewlock->psnext = pslock->psnext;
	pslock->psnext = psnewlock;

	return 0;
}

static int
ilockdelete (const int ihandle, const off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	struct VBLOCK	*pslocktodelete;
	struct VBLOCK	*pslock;
	int		iindexhandle;

	psvbptr = psvbfile[ihandle];
	iindexhandle = psvbptr->iindexhandle;
	pslock = svbfile[iindexhandle].pslockhead;
	/* Sanity check #1.  If it wasn't locked, ignore it! */
	if (!pslock || pslock->trownumber > trownumber) {
		return 0;
	}
	/* Check if deleting first entry in list */
	if (pslock->trownumber == trownumber) {
#ifndef	CISAMLOCKS
		if (pslock->ihandle != ihandle) {
			return ELOCKED;
		}
#endif
		svbfile[iindexhandle].pslockhead = pslock->psnext;
		if (!svbfile[iindexhandle].pslockhead) {
			svbfile[iindexhandle].pslocktail = NULL;
		}
		vvblockfree (pslock);
		return 0;
	}
	/* Position pslock pointer to previous */
	while (pslock->psnext && pslock->psnext->trownumber < trownumber) {
		pslock = pslock->psnext;
	}
	/* Sanity check #2 */
	if (!pslock->psnext || pslock->psnext->trownumber != trownumber) {
		return 0;
	}
	pslocktodelete = pslock->psnext;
#ifndef	CISAMLOCKS
	if (pslocktodelete->ihandle != ihandle) {
		return ELOCKED;
	}
#endif
	pslock->psnext = pslocktodelete->psnext;
	if (pslocktodelete == svbfile[iindexhandle].pslocktail) {
		svbfile[iindexhandle].pslocktail = pslock;
	}
	vvblockfree (pslocktodelete);

	return 0;
}

/* Global functions */

/*
 *	Overview
 *	========
 *	Ideally, I'd prefer using making use of the high bit for the file open
 *	locks and the row locks.  However, this is NOT allowed by Linux.
 *
 *	After a good deal of deliberation (followed by some snooping with good
 *	old strace), I've decided to make the locking strategy 100% compatible
 *	with Informix(R) CISAM 7.24UC2 as it exists for Linux.
 *	When used in 64-bit mode, it simply extends the values accordingly.
 *
 *	Index file locks (ALL locks are on the index file)
 *	==================================================
 *	Non exclusive file open lock
 *		Off:0x7fffffff	Len:0x00000001	Typ:RDLOCK
 *	Exclusive file open lock (i.e. ISEXCLLOCK)
 *		Off:0x7fffffff	Len:0x00000001	Typ:WRLOCK
 *	Enter a primary function - Non modifying
 *		Off:0x00000000	Len:0x3fffffff	Typ:RDLCKW
 *	Enter a primary function - Modifying
 *		Off:0x00000000	Len:0x3fffffff	Typ:WRLCKW
 *	Lock a data row (Add the row number to the offset)
 *		Off:0x40000000	Len:0x00000001	Typ:WRLOCK
 *	Lock *ALL* the data rows (i.e. islock is called)
 *		Off:0x40000000	Len:0x3fffffff	Typ:WRLOCK
 */

int
ivbenter (const int ihandle, const unsigned int imodifying,
	  const unsigned int ispecial)
{
	struct DICTINFO	*psvbptr;
	off_t		tlength;
	size_t		iloop;
	int		ilockmode;
	char		cvbnodetmp[VB_NODE_MAX];

	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		iserrno = EBADARG;
		return -1;
	}
	psvbptr = psvbfile[ihandle];
	if (unlikely(!psvbptr)) {
		iserrno = ENOTOPEN;
		return -1;
	}
	for (iloop = 0; iloop < MAXSUBS; iloop++) {
		if (psvbptr->pskeycurr[iloop]
		    && psvbptr->pskeycurr[iloop]->trownode == -1) {
			psvbptr->pskeycurr[iloop] = NULL;
		}
	}
	iserrno = 0;
	if (psvbptr->iisopen && ivbintrans != VBCOMMIT && ivbintrans != VBROLLBACK) {
		iserrno = ENOTOPEN;
		return -1;
	}
	if ((psvbptr->iopenmode & ISTRANS) && ivbintrans == VBNOTRANS) {
		iserrno = ENOTRANS;
		return -1;
	}
	psvbptr->iindexchanged = 0;
	if (psvbptr->iopenmode & ISEXCLLOCK) {
		psvbptr->iisdictlocked |= 0x01;
		return 0;
	}
	if (psvbptr->iisdictlocked & 0x03) {
		iserrno = EBADARG;
		return -1;
	}
/* RXW
	if (imodifying) {
		ilockmode = VBWRLCKW;
	} else {
		ilockmode = VBRDLCKW;
	}
*/
	if (imodifying) {
		ilockmode = VBWRLOCK;
	} else {
		ilockmode = VBRDLOCK;
	}
	tlength = VB_OFFLEN_3F;
	psvbptr->iindexchanged = 0;
	if (ivblock (psvbptr->iindexhandle, (off_t)0, tlength, ilockmode)) {
		iserrno = EFLOCKED;
		return -1;
	}
	psvbptr->iisdictlocked |= 0x01;
/* RXW
	if (unlikely(ispecial)) {
		if (tvblseek (psvbptr->iindexhandle, (off_t)0, SEEK_SET)) {
			psvbptr->iisdictlocked = 0;
			ivbexit (ihandle);
			iserrno = EBADFILE;
			return -1;
		}
		if (tvbread (psvbptr->iindexhandle, cvbnodetmp, 8) != 8) {
			psvbptr->iisdictlocked = 0;
			ivbexit (ihandle);
			iserrno = EBADFILE;
			return -1;
		}
		psvbptr->inodesize = inl_ldint (&cvbnodetmp[6]) + 1;
	}
*/
	if (ivbblockread (ihandle, 1, (off_t) 1, cvbnodetmp)) {
		psvbptr->iisdictlocked = 0;
		ivbexit (ihandle);
		iserrno = EBADFILE;
		return -1;
	}
	memcpy ((void *)&psvbptr->sdictnode, (void *)cvbnodetmp,
		sizeof (struct DICTNODE));
	psvbptr->iisdictlocked |= 0x01;
/*
 * If we're in C-ISAM mode, then there is NO way to determine if a given node
 * has been updated by some other process.  Thus *ANY* change to the index
 * file needs to invalidate the ENTIRE cache for that table!!!
 * If we're in VBISAM 64-bit mode, then each node in the index table has a
 * stamp on it stating the transaction number when it was updated.  If this
 * stamp is BELOW that of the current transaction number, we know that the
 * associated VBTREE / VBKEY linked lists are still coherent!
 */
#if	ISAMMODE == 0
	if (psvbptr->ttranslast !=
	    inl_ldquad (psvbptr->sdictnode.ctransnumber)) {
		for (iloop = 0; iloop < MAXSUBS; iloop++) {
			if (psvbptr->pstree[iloop]) {
				vvbtreeallfree (ihandle, iloop,
						psvbptr->pstree[iloop]);
			}
			psvbptr->pstree[iloop] = NULL;
		}
	}
#endif
	return 0;
}

int
ivbexit (const int ihandle)
{
	struct DICTINFO	*psvbptr;
	struct VBKEY	*pskey, *pskeycurr;
	off_t		tlength, ttransnumber;
	int		iresult = 0, iloop, iloop2, isaveerror;
	char		cvbnodetmp[VB_NODE_MAX];

	isaveerror = iserrno;
	psvbptr = psvbfile[ihandle];
	if (!psvbptr || (!psvbptr->iisdictlocked && !(psvbptr->iopenmode & ISEXCLLOCK))) {
		iserrno = EBADARG;
		return -1;
	}
	ttransnumber = inl_ldquad (psvbptr->sdictnode.ctransnumber);
	psvbptr->ttranslast = ttransnumber;
/* RXW
	if (psvbptr->iopenmode & ISEXCLLOCK) {
		return 0;
	}
*/
	if (psvbptr->iisdictlocked & 0x02) {
		if (!(psvbptr->iisdictlocked & 0x04)) {
			psvbptr->ttranslast = ttransnumber + 1;
			inl_stquad (ttransnumber + 1,
				    psvbptr->sdictnode.ctransnumber);
		}
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
	tlength = VB_OFFLEN_3F;
	if (ivblock (psvbptr->iindexhandle, (off_t)0, tlength, VBUNLOCK)) {
		iserrno = errno;
		return -1;
	}
	psvbptr->iisdictlocked = 0;
	/* Free up any key/tree no longer wanted */
	for (iloop2 = 0; iloop2 < psvbptr->inkeys; iloop2++) {
		pskey = psvbptr->pskeycurr[iloop2];
		/*
		 * This is a REAL fudge factor...
		 * We simply free up all the dynamically allocated memory
		 * associated with non-current nodes above a certain level.
		 * In other words, we wish to keep the CURRENT key and all data
		 * in memory above it for IVBBUFFERLEVEL levels.
		 * Anything *ABOVE* that in the tree is to be purged with
		 * EXTREME prejudice except for the 'current' tree up to the
		 * root.
		 */
		for (iloop = 0; pskey && iloop < IVBBUFFERLEVEL; iloop++) {
			if (pskey->psparent->psparent) {
				pskey = pskey->psparent->psparent->pskeycurr;
			} else {
				pskey = NULL;
			}
		}
		if (!pskey) {
			iserrno = isaveerror;
			return 0;
		}
		while (pskey) {
			for (pskeycurr = pskey->psparent->pskeyfirst; pskeycurr;
			     pskeycurr = pskeycurr->psnext) {
				if (pskeycurr != pskey && pskeycurr->pschild) {
					vvbtreeallfree (ihandle, iloop2, pskeycurr->pschild);
					pskeycurr->pschild = NULL;
				}
			}
			if (pskey->psparent->psparent) {
				pskey = pskey->psparent->psparent->pskeycurr;
			} else {
				pskey = NULL;
			}
		}
	}
	if (iresult) {
		return -1;
	}
	iserrno = isaveerror;
	return 0;
}

int
ivbfileopenlock (const int ihandle, const int imode)
{
	struct DICTINFO	*psvbptr;
	off_t		toffset;
	int		ilocktype;
/* RXW
	int		iresult;
*/
	char		cvbnodetmp[VB_NODE_MAX];

	/* Sanity check - Is ihandle a currently open table? */
	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		return ENOTOPEN;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr) {
		return ENOTOPEN;
	}

	toffset = VB_OFFLEN_7F;

	switch (imode) {
	case 0:
		ilocktype = VBUNLOCK;
		break;

	case 1:
		ilocktype = VBRDLOCK;
		break;

	case 2:
		ilocktype = VBWRLOCK;
		break;

	default:
		return EBADARG;
	}
	if (ivblock (psvbptr->iindexhandle, toffset, (off_t)1, ilocktype)) {
		return EFLOCKED;
	}
	if (imode == 2) {
		if (ivbblockread (ihandle, 1, (off_t) 1, cvbnodetmp)) {
			(void)ivblock (psvbptr->iindexhandle, toffset, (off_t)1, VBUNLOCK);
			return EBADFILE;
		}
		memcpy ((void *)&psvbptr->sdictnode, (void *)cvbnodetmp,
			sizeof (struct DICTNODE));
	}
	return 0;
}

int
ivbdatalock (const int ihandle, const int imode, const off_t trownumber)
{
	struct DICTINFO	*psvbptr;
	struct VBLOCK	*pslock;
	off_t		tlength = 1, toffset;
	int		iresult = 0;

	/* Sanity check - Is ihandle a currently open table? */
	if (unlikely(ihandle < 0 || ihandle > ivbmaxusedhandle)) {
		return ENOTOPEN;
	}
	psvbptr = psvbfile[ihandle];
	if (!psvbptr) {
		return ENOTOPEN;
	}
	if (psvbptr->iopenmode & ISEXCLLOCK) {
		return 0;
	}
	/*
	 * If this is a FILE (un)lock (row = 0), then we may as well free ALL
	 * locks. Even if CISAMLOCKS is set, we do this!
	 */
	if (trownumber == 0) {
		for (pslock = svbfile[psvbptr->iindexhandle].pslockhead; pslock;
		     pslock = pslock->psnext) {
			ivbdatalock (ihandle, VBUNLOCK, pslock->trownumber);
		}
		tlength = VB_OFFLEN_3F;
		if (imode == VBUNLOCK) {
			psvbptr->iisdatalocked = 0;
		} else {
			psvbptr->iisdatalocked = 1;
		}
	} else if (imode == VBUNLOCK) {
		iresult = ilockdelete (ihandle, trownumber);
	}
	if (!iresult) {
		toffset = VB_OFFLEN_40;
		iresult = ivblock (psvbptr->iindexhandle, toffset + trownumber,
				tlength, imode);
		if (iresult != 0) {
			return ELOCKED;
		}
	}
	if ((imode != VBUNLOCK) && trownumber) {
		return ilockinsert (ihandle, trownumber);
	}
	return iresult;
}
